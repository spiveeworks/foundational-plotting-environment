#ifndef FPE_DESERIALIZE_H
#define FPE_DESERIALIZE_H

#include <stdint.h>

#include "calculation.h"
#include "plotter.h"

typedef uint8_t byte;

void initialise_nonblocking_stdin(void);
int nonblocking_stdin(byte *buff, int cap);

bool nonblocking_stdin_initialised;
bool async_stdin_started;

#ifdef _WIN32
  HANDLE stdin_handle;
  void initialise_nonblocking_stdin(void) {
      stdin_handle = GetStdHandle(STD_INPUT_HANDLE);

      nonblocking_stdin_initialised = true;
  }

  OVERLAPPED async_stdin_overlapped;
  void start_async_stdin(byte *buff, int cap) {
      if (!nonblocking_stdin_initialised) initialise_nonblocking_stdin();
      DWORD out;
      BOOL r = ReadFile(stdin_handle, buff, cap, &out, &async_stdin_overlapped);
      if (r) {
        fputs("Got input during kickoff!\n", stderr);
        fflush(stdout);
      }
      async_stdin_started = true;
  }
  bool check_async_stdin(int *count) {
      DWORD out_word = 0;
      BOOL result = GetOverlappedResult(
          stdin_handle,
          &async_stdin_overlapped,
          &out_word,
          false
      );
      if (count) *count = out_word;
      if (result) async_stdin_started = false;
      return result;
  }
#else /* End of Windows, start of unix. */
  #include <unistd.h>

  void initialise_nonblocking_stdin(void) {
      /* Haven't actually tried this on linux, but it definitely doesn't work
         on cygwin. */
      int flags = fcntl(0, F_GETFD);
      fcntl(0, F_SETFD, flags | O_NONBLOCK);

      nonblocking_stdin_initialised = true;
  }

  byte *stdin_buff;
  int stdin_cap;
  void start_async_stdin(byte *buff, int cap) {
      if (!nonblocking_stdin_initialised) initialise_nonblocking_stdin();

      stdin_buff = buff;
      stdin_cap = cap;
      async_stdin_started = true;
  }
  bool check_async_stdin(int *count) {
      int out = read(0, stdin_buff, stdin_cap);
      if (out > 0) {
          async_stdin_started = false;
          if (count) *count = out;
          return true;
      } else {
          return false;
      }
  }
#endif

byte *buffer;
int buffer_start;
int buffer_count;
/* buffer_start + buffer_count <= buffer_cap */
int buffer_cap;

/* Guarantees that there are at least size_needed bytes of spare room
   at the end of the buffer. */
void prepare_buffer(int size_needed) {
    if (buffer_cap - buffer_count < size_needed) {
        int new_cap = buffer_cap + size_needed;
        byte *new_buffer = malloc(new_cap);
        memcpy(new_buffer, buffer + buffer_start, buffer_count);

        buffer = new_buffer;
        buffer_start = 0;
        buffer_cap = new_cap;
    } else if (buffer_cap - (buffer_start + buffer_count) < size_needed) {
        memmove(buffer, buffer + buffer_start, buffer_count);
        buffer_start = 0;
    }
}

void poll_stdin(void) {
    if (!async_stdin_started) {
        prepare_buffer(16);

        int buffer_next = buffer_start + buffer_count;
        int remaining = buffer_cap - buffer_next;
        byte *next = buffer + buffer_next;

        start_async_stdin(next, remaining);
    }
    int count = 0;
    if (check_async_stdin(&count)) {
        buffer_count += count;
    }
}

/***************************/
/* Actual Input Processing */
/***************************/

int64 try_read_int7x(byte **next_p, byte *end) {
    byte *next = *next_p;
    if (!next) return 0;

    uint64 result = 0;
    int shift = 0;
    while (true) {
        if (next == end) {
            *next_p = NULL;
            return 0;
        }
        byte c = *next++;
        result |= (uint64)(c & 0x7F) << shift;
        shift += 7;

        if (!(c & 0x80)) {
            *next_p = next;

            /* Sign extend result. */
            if (c & 0x40) result |= (~0) << shift;
            return (int64)result;
        }
    }
}

enum deserialize_mode {
    /* DE_NULL, */
    DE_NEUTRAL,
    DE_INITIAL_STATE,
    DE_CONSTRUCT_FUN, /* Deserialize the 'construct' function. */
    DE_CURVE_PARAMS,
    DE_CURVE_FUN,
};

struct deserialize_state {
    enum deserialize_mode mode;

    int64 *state_vars;
    int state_var_count;
    int state_var_set_count;

    bool is_vertical;
    struct plot_object_parameter *curve_params;
    int curve_param_count;
    int curve_param_set_count;

    int instruction_count; /* Number of instructions needed before builder is
                              complete. */
    struct function_builder builder;
};

enum deserialize_command {
    DE_COMMAND_NIL,
    DE_COMMAND_NEW_CONSTRUCT,
    DE_COMMAND_ADD_FREE_POINT,
    DE_COMMAND_ADD_STATIC_POINT,
    DE_COMMAND_ADD_HORIZONTAL_AXIS,
    DE_COMMAND_ADD_VERTICAL_AXIS,
    DE_COMMAND_ADD_HORIZONTAL_CURVE,
    DE_COMMAND_ADD_VERTICAL_CURVE,
};

struct plot_object_parameter try_deserialize_plot_object_param(
    byte **next_p,
    byte *end,
    int max_val_index
) {
    struct plot_object_parameter result = {true};
    byte *next = *next_p;
    if (!next) return result;

    if (next == end) {
        *next_p = NULL;
        return result;
    }
    result.is_constant = *next++;
    if (result.is_constant != 0 && result.is_constant != 1) exit(EXIT_FAILURE);
    int64 val = try_read_int7x(&next, end);
    if (!next) {
        *next_p = NULL;
        return result;
    }
    if (result.is_constant) result.constant_val = val;
    else if (val < 0 || val >= max_val_index) exit(EXIT_FAILURE);
    else result.constant_val = val;

    *next_p = next;
    return result;
}

void try_deserialize_instruction(
    struct function_builder *builder,
    byte **next_p,
    byte *end
) {
    byte *next = *next_p;
    if (!next) return;

    struct instruction instr;
    if (next == end) {
        *next_p = NULL;
        return;
    }
    enum operation op = *next++;
    instr.op = op;
    bool imm1 = (op & OP_IMM1) != 0;
    bool imm2 = (op & OP_IMM2) != 0;
    op = op & 63; /* 00111111 */

    int val_count = builder->arg_count
        + builder->intermediates_count;
    if (op == OP_SELECT) {
        int64 arg1 = try_read_int7x(&next, end);
        int64 arg2 = try_read_int7x(&next, end);
        int64 arg3 = try_read_int7x(&next, end);
        if (!next) {
            *next_p = NULL;
            return;
        }
        if (!imm1 && (arg1 < 0 || arg1 >= val_count)) exit(EXIT_FAILURE);
        if (!imm2 && (arg2 < 0 || arg2 >= val_count)) exit(EXIT_FAILURE);
        if (arg3 < 0 || arg3 >= val_count) exit(EXIT_FAILURE);

        instr.args = malloc(3 * sizeof(int64));
        instr.args[0] = arg1;
        instr.args[1] = arg2;
        instr.args[2] = arg3;
    } else {
        int64 arg1 = try_read_int7x(&next, end);
        if (!imm1 && (arg1 < 0 || arg1 >= val_count)) exit(EXIT_FAILURE);
        instr.binary.arg1 = arg1;
        if (op != OP_MOV && op != OP_NEG && op != OP_ILOG) {
            int64 arg2 = try_read_int7x(&next, end);
            if (!imm2 && (arg2 < 0 || arg2 >= val_count)) exit(EXIT_FAILURE);
            instr.binary.arg2 = arg2;
        }
        if (!next) {
            *next_p = NULL;
            return;
        }
    }

    *next_p = next;
    function_builder_push(builder, &instr);
}

void try_deserialize_input(
    struct plotter_state *plotter,
    struct deserialize_state *de
) {
    poll_stdin();

    byte *start = buffer + buffer_start;
    byte *end = start + buffer_count;
    byte *next = start;

    while (true) {
        switch(de->mode) {
        case DE_NEUTRAL:
          {
            if (next == end) return;
            enum deserialize_command command = *next++;
            switch (command) {
            case DE_COMMAND_NEW_CONSTRUCT:
              {
                de->state_var_count = try_read_int7x(&next, end);
                if (!next) return;
                de->state_vars = malloc(de->state_var_count * sizeof(int64));
                de->state_var_set_count = 0;
                de->mode = DE_INITIAL_STATE;
                break;
              }
            case DE_COMMAND_ADD_FREE_POINT:
            case DE_COMMAND_ADD_STATIC_POINT:
              {
                int val_count = plotter->update_and_construct.arg_count +
                    plotter->update_and_construct.intermediates_count;
                struct plot_object_parameter paramx =
                    try_deserialize_plot_object_param(&next, end, val_count);
                struct plot_object_parameter paramy =
                    try_deserialize_plot_object_param(&next, end, val_count);
                if (!next) return;
                bool is_free = command == DE_COMMAND_ADD_FREE_POINT;
                plotter_add_point(plotter, is_free, paramx, paramy);
                break;
              }
            case DE_COMMAND_ADD_HORIZONTAL_AXIS:
            case DE_COMMAND_ADD_VERTICAL_AXIS:
              {
                int val_count = plotter->update_and_construct.arg_count +
                    plotter->update_and_construct.intermediates_count;
                struct plot_object_parameter paramc =
                    try_deserialize_plot_object_param(&next, end, val_count);
                if (!next) return;
                bool is_vertical = command == DE_COMMAND_ADD_VERTICAL_AXIS;

                plotter_add_axis(plotter, is_vertical, paramc);
                break;
              }
            case DE_COMMAND_ADD_HORIZONTAL_CURVE:
            case DE_COMMAND_ADD_VERTICAL_CURVE:
                de->curve_param_count = try_read_int7x(&next, end);
                if (!next) return;
                de->curve_params = malloc(
                    de->curve_param_count * sizeof(struct plot_object_parameter)
                );
                de->curve_param_set_count = 0;
                de->is_vertical = command == DE_COMMAND_ADD_VERTICAL_CURVE;
                de->mode = DE_CURVE_PARAMS;
                break;
            default:
              exit(EXIT_FAILURE);
            }
            break;
          }
        case DE_INITIAL_STATE:
            if (de->state_var_set_count == de->state_var_count) {
                de->instruction_count = try_read_int7x(&next, end);
                if (!next) return;

                de->builder =
                    create_function_builder(de->state_var_count);
                de->mode = DE_CONSTRUCT_FUN;
            } else {
                int64 val = try_read_int7x(&next, end);
                if (!next) return;
                de->state_vars[de->state_var_set_count] = val;
                de->state_var_set_count++;
            }
            break;
        case DE_CURVE_PARAMS:
            if (de->curve_param_set_count == de->curve_param_count) {
                de->instruction_count = try_read_int7x(&next, end);
                if (!next) return;

                de->builder =
                    create_function_builder(de->curve_param_count + 1);
                de->mode = DE_CURVE_FUN;
            } else {
                int val_count = plotter->update_and_construct.arg_count +
                    plotter->update_and_construct.intermediates_count;
                struct plot_object_parameter param =
                    try_deserialize_plot_object_param(&next, end, val_count);
                if (!next) return;
                de->curve_params[de->curve_param_set_count] = param;
                de->curve_param_set_count++;
            }
            break;
        case DE_CONSTRUCT_FUN:
            if (de->builder.instruction_count == de->instruction_count) {
                destroy_plotter(plotter);

                plotter->state_var_count = de->state_var_count;
                plotter->state_vars = de->state_vars;
                plotter->update_and_construct = build_function(
                    &de->builder,
                    de->state_var_count
                );
                plotter->plot_objects = NULL;
                plotter->plot_object_count = 0;

                de->mode = DE_NEUTRAL;
            } else {
                try_deserialize_instruction(&de->builder, &next, end);
                if (!next) return;
            }
            break;
        case DE_CURVE_FUN:
            if (de->builder.instruction_count == de->instruction_count) {
                struct plot_object curve;
                curve.type = PLOT_FUNCTION;
                curve.is_vertical = de->is_vertical;
                curve.params = de->curve_params;
                curve.param_count = de->curve_param_count;
                curve.function = build_function(&de->builder, 1);
                plotter_add_object(plotter, curve);

                de->mode = DE_NEUTRAL;
            } else {
                try_deserialize_instruction(&de->builder, &next, end);
                if (!next) return;
            }
            break;
        }

        if (!next) return;
        /* Confusing line... */
        int consumed = next - start;
        if (consumed < 0 || consumed > buffer_count) exit(EXIT_FAILURE);
        buffer_start += consumed;
        buffer_count -= consumed;
        start = next;
    }
}

#endif
