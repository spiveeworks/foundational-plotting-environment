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
        printf("got %d bytes\n", count);
        fflush(stdout);
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
    DE_CONSTRUCT_FUN,
};

struct deserialize_state {
    enum deserialize_mode mode;
    int instruction_count; /* Number of instructions needed before builder is
                              complete. */
    int state_var_count;
    struct function_builder builder;
};

enum deserialize_command {
    DE_COMMAND_NIL,
    DE_COMMAND_NEW_CONSTRUCT,
    DE_COMMAND_ADD_PLOT_OBJECT,
};

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
                de->instruction_count = try_read_int7x(&next, end);
                if (!next) return;

                de->builder = create_function_builder(de->state_var_count);
                printf("beginning function build\n");
                fflush(stdout);
                de->mode = DE_CONSTRUCT_FUN;
                break;
              }
            case DE_COMMAND_ADD_PLOT_OBJECT:
              {
                break;
              }
            default:
              exit(EXIT_FAILURE);
            }
            break;
          }
        case DE_CONSTRUCT_FUN:
          {
            if (de->builder.instruction_count == de->instruction_count) {
                printf("ending function build\n");
                fflush(stdout);
                destroy_plotter(plotter);

                plotter->state_var_count = de->state_var_count;
                plotter->state_vars =
                    calloc(de->state_var_count, sizeof(int64));
                plotter->update_and_construct = build_function(
                    &de->builder,
                    de->state_var_count
                );
                plotter->plot_objects = NULL;
                plotter->plot_object_count = 0;

                de->mode = DE_NEUTRAL;
                break;
            }
            struct instruction instr;
            if (next == end) return;
            enum operation op = *next++;
            instr.op = op;
            if (op == OP_SELECT) {
                int arg1 = try_read_int7x(&next, end);
                int arg2 = try_read_int7x(&next, end);
                int arg3 = try_read_int7x(&next, end);
                if (!next) return;

                instr.args = malloc(3 * sizeof(int64));
            } else {
                instr.binary.arg1 = try_read_int7x(&next, end);
                if (op != OP_MOV && op != OP_NEG && op != OP_ILOG) {
                    instr.binary.arg2 = try_read_int7x(&next, end);
                }
                if (!next) return;
            }

            printf("got operation\n");
            fflush(stdout);

            function_builder_push(&de->builder, &instr);
            break;
          }
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
