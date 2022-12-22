#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "window.h"
#include "entrypoint.h"

#include "calculation.h"

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
      //ReadFile(stdin_handle, buff, cap, &out, &async_stdin_overlapped);
      BOOL r = ReadFile(stdin_handle, buff, cap, &out, &async_stdin_overlapped);
      if (r) {
        fputs("Got input during kickoff!\n", stdout);
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
        fputs("Got input: ", stdout);
        fwrite(buffer + buffer_start + buffer_count, 1, count, stdout);
        fflush(stdout);

        buffer_count += count;
    }
}

void draw_frame(long frame, struct window *win) {
    ImageView out = win->back_buf;
    char *row = out.data;
    for (int j = 0; j < out.height; j++) {
        unsigned *pel = (unsigned*)row;
        unsigned *end = pel + out.width;
        for (int i = 0; i < out.width; i++) {
            unsigned t = (frame + i + j) % 511;
            if (t >= 256) t = 511 - t;
            *pel = (t << 16) | (t << 8) | (255 - t);
            pel++;
        }
        while (pel < end) *pel++ = 0x888888;
        row += out.stride;
    }

    win->back_buffer_drawn = true;
}

void test_functions(void) {
    struct function_builder builder = create_function_builder(2);
    struct instruction instr;

    /* x2 = x0 * x0 */
    instr.op = OP_MUL;
    instr.binary.arg1 = 0;
    instr.binary.arg2 = 0;
    function_builder_push(&builder, &instr);

    /* x3 = x1 * x1 */
    instr.op = OP_MUL;
    instr.binary.arg1 = 1;
    instr.binary.arg2 = 1;
    function_builder_push(&builder, &instr);

    /* x4 = x2 + x3 */
    instr.op = OP_ADD;
    instr.binary.arg1 = 2;
    instr.binary.arg2 = 3;
    function_builder_push(&builder, &instr);

    /* x5 = ilog(1000000) = 19*/
    instr.op = OP_ILOG | OP_IMM1;
    instr.binary.arg1 = 1000000;
    function_builder_push(&builder, &instr);

    struct function f = build_function(&builder, 2);

    f.values[0] = 30;
    f.values[1] = 40;
    int64 *ys = calculate_function(&f);
    printf("Got result %lld\n", ys[0]);
    fflush(stdout);

    destroy_function(&f);
    /* Does nothing. Just testing that it doesn't double free. */
    destroy_function_builder(&builder);
}

int entry_point(int argc, char **argv) {
    test_functions();

    struct window win;
    create_window(&win, "Settlement WinPort", false);
    fputs("Window created.\n", stdout);
    fflush(stdout);

    long frame = 0;
    while (true) {
        struct window_event event;
        if (get_event(&win, &event, false)) {
            if (event.type == WINDOW_CLOSE) {
                fputs("Window closed.\n", stdout);
                fflush(stdout);
                exit(EXIT_SUCCESS);
            }
            continue;
        }

        poll_stdin();

        draw_frame(frame, &win);
        flush_window(&win);

        frame++;
    }

    return 0;
}
