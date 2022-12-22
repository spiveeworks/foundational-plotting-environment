#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "window.h"
#include "entrypoint.h"

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

int entry_point(int argc, char **argv) {
    struct window win;
    create_window(&win, "Settlement WinPort", false);
    fputs("Window created.\n", stdout);
    fflush(stdout);

    struct camera camera = {0};
    camera.zoom = 1;

    struct plotter_state plotter = {0};
    plotter.state_var_count = 2;
    int64 state[2] = {0, 0};
    plotter.state_vars = state;
    {
        struct function_builder builder = create_function_builder(2);
        plotter.update_and_construct = build_function(&builder, 2);
    }
    struct plot_object plot_objects[3];
    plot_objects[0].type = PLOT_FREE_POINT;
    plot_objects[0].params = malloc(2 * sizeof(struct plot_object_parameter));
    plot_objects[0].param_count = 2;
    plot_objects[0].params[0].is_constant = false;
    plot_objects[0].params[0].val_index = 0;
    plot_objects[0].params[1].is_constant = false;
    plot_objects[0].params[1].val_index = 1;
    plot_objects[1].type = PLOT_STATIC_POINT;
    plot_objects[1].params = malloc(2 * sizeof(struct plot_object_parameter));
    plot_objects[1].param_count = 2;
    plot_objects[1].params[0].is_constant = true;
    plot_objects[1].params[0].constant_val = 0;
    plot_objects[1].params[1].is_constant = false;
    plot_objects[1].params[1].val_index = 1;
    plot_objects[2].type = PLOT_STATIC_POINT;
    plot_objects[2].params = malloc(2 * sizeof(struct plot_object_parameter));
    plot_objects[2].param_count = 2;
    plot_objects[2].params[0].is_constant = false;
    plot_objects[2].params[0].val_index = 0;
    plot_objects[2].params[1].is_constant = true;
    plot_objects[2].params[1].constant_val = 0;

    plotter.plot_objects = plot_objects;
    plotter.plot_object_count = 3;

    int selected_object = -1;

    long frame = 0;
    while (true) {
        struct window_event event;
        if (get_event(&win, &event, false)) {
            if (event.type == WINDOW_CLOSE) {
                fputs("Window closed.\n", stdout);
                fflush(stdout);
                exit(EXIT_SUCCESS);
            } else if (event.type == WINDOW_BUTTON_DOWN) {
                int64 min_screen_quadrance = 100;
                selected_object = -1;
                for (int i = 0; i < plotter.plot_object_count; i++) {
                    struct plot_object *it = &plotter.plot_objects[i];
                    if (it->type != PLOT_FREE_POINT) {
                        continue;
                    }

                    int64 x = it->params[0].prev_val;
                    int64 y = it->params[1].prev_val;
                    int sx =  win.width / 2 + (x - camera.centre_x) / camera.zoom;
                    int sy = win.height / 2 - (y - camera.centre_y) / camera.zoom;
                    int dx = sx - win.mouse_x;
                    int dy = sy - win.mouse_y;
                    int64 screen_quadrance = dx * dx + dy * dy;
                    if (screen_quadrance < min_screen_quadrance) {
                        min_screen_quadrance = screen_quadrance;
                        selected_object = i;
                    }
                }
            } else if (event.type == WINDOW_BUTTON_UP) {
                selected_object = -1;
            }
            continue;
        }

        poll_stdin();

        int64 pos_x = camera.centre_x + (win.mouse_x - win.width / 2) * camera.zoom;
        int64 pos_y = camera.centre_y + (win.height / 2 - win.mouse_y) * camera.zoom;
        update_plotter_state(&plotter, selected_object, pos_x, pos_y);
        draw_plotter_objects(&win.back_buf, &camera, &plotter);

        flush_window(&win);

        frame++;
    }

    return 0;
}
