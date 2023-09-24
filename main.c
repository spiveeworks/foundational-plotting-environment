#include <stdio.h>
#include <stdbool.h>

#include "window.h"
#include "entrypoint.h"

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
    create_window(&win, "Plotting Sandbox", false);

    long frame = 0;
    while (true) {
        struct window_event event;
        if (get_event(&win, &event, false)) {
            if (event.type == WINDOW_CLOSE) {
                exit(EXIT_SUCCESS);
            }
            continue;
        }

        draw_frame(frame, &win);
        flush_window(&win);

        frame++;
    }

    return 0;
}
