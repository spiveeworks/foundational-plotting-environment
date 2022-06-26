#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdbool.h>
#include <error.h>
#include <errno.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

#include "draw.h"

int window_width = 600;
int window_height = 600;

long centre_x = 14142 / 2;
long centre_y = 10000;

long zoom = 64;

void screen_to_plane(int i, int j, long *x_out, long *y_out) {
    if (x_out) *x_out = centre_x + zoom * (i - window_width / 2);
    if (y_out) *y_out = centre_y - zoom * (j - window_height / 2);
}

void plane_to_screen(long x, long y, int *i_out, int *j_out) {
    if (i_out) *i_out = window_width / 2 + (x - centre_x) / zoom;
    if (j_out) *j_out = window_height / 2 - (y - centre_y) / zoom;
}

void draw_horizontal_line(XImage *out, long y) {
    char *data = out->data + out->xoffset;
    int stride = out->bytes_per_line;

    int j = window_height / 2 - (y - centre_y) / zoom;
    if (j >= 0 && j < window_height) {
        unsigned *target_row = (unsigned*)&data[j * stride];
        for (int i = 0; i < window_width; i++) target_row[i] = 0x888888;
    }
}

void draw_vertical_line(XImage *out, long x) {
    char *data = out->data + out->xoffset;
    int stride = out->bytes_per_line;

    int i = window_width / 2 + (x - centre_x) / zoom;
    if (i >= 0 && i < window_width) {
        for (int j = 0; j < window_height; j++) {
            unsigned *row = (unsigned*)data;
            row[i] = 0x888888;
            data += stride;
        }
    }
}

void draw_points(XImage *out, long colour, int count, long *xs, long *ys) {
    char *data = out->data + out->xoffset;
    int stride = out->bytes_per_line;

    for (int ind = 0; ind < count; ind++) {
        int i =  window_width / 2 + (xs[ind] - centre_x) / zoom;
        int j = window_height / 2 - (ys[ind] - centre_y) / zoom;

        if (i < 1) continue;
        if (i >= out->width - 1) continue;
        if (j < 1) continue;
        if (j >= out->height - 1) continue;

        unsigned *target_row = (unsigned*)&data[stride * (j-1)];
        target_row[i] = colour;
        target_row = (unsigned*)((char*)target_row + stride);
        target_row[i-1] = colour;
        target_row[i] = colour;
        target_row[i+1] = colour;
        target_row = (unsigned*)((char*)target_row + stride);
        target_row[i] = colour;
    }
}

struct polynomial {
    long *coefficients;
    long denominator;
    int order;
};

#define QUAD_ORDER 2
long quad_coefficients[QUAD_ORDER + 1] = {0, 0, 10000};
struct polynomial quad = {quad_coefficients, 10000, QUAD_ORDER};

#define APPROX_ORDER 3
long approx_coefficients[APPROX_ORDER + 1] = {9990, 5000, -1250, 625};
struct polynomial approx = {approx_coefficients, 10000, APPROX_ORDER};

long eval_polynomial(struct polynomial p, long x) {
    if (p.order == 0) return p.coefficients[0] / p.denominator;
    /* else */

    long result = p.coefficients[p.order];
    for (int i = p.order - 1; i >= 0; i--) {
        result *= x;
        result /= p.denominator;
        result += p.coefficients[i];
    }
    return result;
}

long bisect_polynomial(struct polynomial p, long y, long below, long above) {
    if (eval_polynomial(p, below) > eval_polynomial(p, above)) {
        long tmp = above;
        above = below;
        below = tmp;
    }
    while (above > below + 1 || above < below - 1) {
        long x = (below + above) / 2;
        long px = eval_polynomial(p, x);
        if (px < y) below = x;
        else above = x;
    }
    return below;
}

int main(int argc, char **argv) {
    bool benchmark = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-benchmark") == 0) {
            if (benchmark) fprintf(stderr, "Warning: \"-bench\" option appeared more than once.\n");
            benchmark = true;
        } else {
            fprintf(stderr, "Warning: \"%s\" is not a known command line argument.\n", argv[i]);
        }
    }

    init_text();

    bool key_down[256] = {};

    Display *dis = XOpenDisplay(NULL);

    if (!XShmQueryExtension(dis)) {
        fprintf(stderr, "Error: The shared memory extension is required for this program to run.\n");
        exit(EXIT_FAILURE);
    }

    Window win = XCreateSimpleWindow(dis, DefaultRootWindow(dis), 0, 0,
        600, 600, 0, 0xFFFFFF, 0x000000);
    XSetStandardProperties(dis, win,
        "Foundational Plotting Envrionment",
        "Foundational Plotting Environment",
        None, NULL, 0, NULL);

    long mask = StructureNotifyMask | ButtonPressMask | ButtonReleaseMask
        | KeyPressMask | KeyReleaseMask | PointerMotionMask | FocusChangeMask
        | StructureNotifyMask;
    XSelectInput(dis, win, mask);

    GC gc = XCreateGC(dis, win, 0, 0);

    XClearWindow(dis, win);
    XMapRaised(dis, win);

    bool back_buf_resize = false;
    XImage *back_buf = NULL;
    XImage *unscaled_buf = NULL;
    XShmSegmentInfo shm_info;

    /* We could ask X for the initial mouse position. */
    int mouse_i = 0;
    int mouse_j = 0;
    long mouse_x = 0;
    long mouse_y = 0;

    bool waiting_for_shm_completion = false;
    const int SHM_COMPLETION = XShmGetEventBase (dis) + ShmCompletion;

    bool redraw = false;

    bool panning = false;
    long pan_x;
    long pan_y;

    while (true) {
        XEvent event;
        XNextEvent(dis, &event);

        if (event.type == ConfigureNotify) {
            back_buf_resize = true;
            redraw = true;
        } else if (event.type == SHM_COMPLETION) {
            waiting_for_shm_completion = false;
        } else if (event.type == KeyPress) {
            KeySym key;
            char text[64];

            if (XLookupString(&event.xkey, text, 255, &key, 0) == 1) {
                if (key == XK_Escape) exit(EXIT_SUCCESS);
            }
        } else if (event.type == KeyRelease) {
            /* Do nothing. */
        } else if (event.type == ButtonPress) {
            int button = event.xbutton.button;
            if (button == 1) {
                panning = true;

                mouse_i = event.xbutton.x;
                mouse_j = event.xbutton.y;
                screen_to_plane(mouse_i, mouse_j, &mouse_x, &mouse_y);
            } else if (button == 4) {
                zoom = zoom * 4 / 5;
                if (zoom == 0) zoom = 1;

                long new_mouse_x, new_mouse_y;
                screen_to_plane(mouse_i, mouse_j, &new_mouse_x, &new_mouse_y);
                centre_x -= new_mouse_x - mouse_x;
                centre_y -= new_mouse_y - mouse_y;

                redraw = true;
            } else if (button == 5) {
                zoom = zoom * 5 / 4;
                if (zoom < 5) zoom += 1;

                long new_mouse_x, new_mouse_y;
                screen_to_plane(mouse_i, mouse_j, &new_mouse_x, &new_mouse_y);
                centre_x -= new_mouse_x - mouse_x;
                centre_y -= new_mouse_y - mouse_y;

                redraw = true;
            }
        } else if (event.type == ButtonRelease) {
            int button = event.xbutton.button;
            if (button == 1) {
                panning = false;
            }
        } else if (event.type == MotionNotify) {
            mouse_i = event.xmotion.x;
            mouse_j = event.xmotion.y;
            if (panning) {
                long new_mouse_x, new_mouse_y;
                screen_to_plane(mouse_i, mouse_j, &new_mouse_x, &new_mouse_y);
                centre_x -= new_mouse_x - mouse_x;
                centre_y -= new_mouse_y - mouse_y;
                redraw = true;
            } else {
                screen_to_plane(mouse_i, mouse_j, &mouse_x, &mouse_y);
            }
        } else if (event.type == FocusOut) {
            for (int i = 0; i < 256; i++) key_down[i] = false;
        } else if (event.type == DestroyNotify) {
            exit(EXIT_SUCCESS);
        }

        if (!back_buf && !back_buf_resize) continue;
        if (waiting_for_shm_completion) continue;
        if (XEventsQueued(dis, QueuedAlready) > 0) continue;
        if (!redraw) continue;

        if (!back_buf || back_buf_resize) {
            if (back_buf) {
                XShmDetach(dis, &shm_info);
                XDestroyImage(back_buf);
                shmdt(shm_info.shmaddr);
                shmctl(shm_info.shmid, IPC_RMID, 0);
            }
            unsigned depth;
            /* These are mostly unused, but we have to write them
               somewhere. */
            Window root;
            int offsetx;
            int offsety;
            unsigned border;

            XGetGeometry(dis, win, &root, &offsetx, &offsety,
                &window_width, &window_height, &border, &depth);

            Visual *visual;
            {
                XVisualInfo attr;
                long mask = VisualClassMask | VisualDepthMask
                    | VisualRedMaskMask | VisualGreenMaskMask
                    | VisualBlueMaskMask;
                attr.class = TrueColor;
                attr.depth = 24;
                attr.red_mask = 0xFF0000;
                attr.green_mask = 0x00FF00;
                attr.blue_mask = 0x0000FF;

                int format_count;
                XVisualInfo *formats =
                    XGetVisualInfo(dis, mask, &attr, &format_count);
                if (format_count == 0) {
                    fprintf(stderr, "Error: Currently only devices with 24-bit RGB colour are supported.\n");
                    exit(EXIT_FAILURE);
                }

                visual = formats[0].visual;

                XFree(formats);
            }

            /* Seems to crash if we don't wait for shm completion between
               creating and recreating. Dunno why, but good to note. */
            back_buf = XShmCreateImage(dis, visual, 24, ZPixmap, NULL,
                 &shm_info, window_width, window_height);
            shm_info.shmid = shmget (IPC_PRIVATE,
                back_buf->bytes_per_line * back_buf->height, IPC_CREAT|0777);
            shm_info.shmaddr = back_buf->data = shmat(shm_info.shmid, 0, 0);
            shm_info.readOnly = true;
            XShmAttach(dis, &shm_info);

            back_buf_resize = false;
        }

        struct timespec start;
        clock_gettime(CLOCK_MONOTONIC_RAW, &start);

        /* Black background */
        fill_rectangle(back_buf, 0, 0, window_width, window_height, (RGBA){0, 0, 0});

        /* Axes */
        draw_horizontal_line(back_buf, 0);
        draw_vertical_line(back_buf, 0);

        /* Plots */
        long xs[window_width];
        for (int i = 0; i < window_width; i++) {
            xs[i] = centre_x + zoom * (i - window_width / 2);
        }

        long sqrt_ys[window_width];
        for (int i = 0; i < window_width; i++) {
            sqrt_ys[i] = bisect_polynomial(quad, xs[i] + 10000, -20000, xs[i] + 30000);
        }
        draw_points(back_buf, 0xFFFFFF, window_width, xs, sqrt_ys);

        long poly_ys[window_width];
        for (int i = 0; i < window_width; i++) {
            long x = xs[i] + 10000;
            int upscale = 0;
            int downscale = 0;
            while (x > 20000) {
                x /= 4;
                upscale += 1;
            }
            while (x < 5000 && x > 1) {
                x *= 4;
                downscale += 1;
            }
            long y = eval_polynomial(approx, x - 10000);
            y <<= upscale;
            y >>= downscale;
            poly_ys[i] = y;
        }
        draw_points(back_buf, 0x0000FF, window_width, xs, poly_ys);

        for (int i = 0; i < window_width; i++) poly_ys[i] -= sqrt_ys[i];
        draw_points(back_buf, 0xFF0000, window_width, xs, poly_ys);

        /* Coordinates */
        static char text_data[25];

        int len = snprintf(text_data, 100, "%ld", centre_x);
        draw_string(back_buf, 5, window_height / 2, (str){text_data, len});

        len = snprintf(text_data, 100, "%ld", centre_y);
        draw_string(back_buf, window_width / 2, 15, (str){text_data, len});

        /* Benchmark */
        struct timespec end;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end);

        unsigned long elapsed = (end.tv_sec - start.tv_sec) * 1000000000
            + (end.tv_nsec - start.tv_nsec);
        if (benchmark) printf("Drawing took %dms.\n", elapsed / 1000000);

        /* Flush */
        XShmPutImage (dis, win, gc, back_buf, 0, 0, 0, 0, window_width, window_height, true);
        waiting_for_shm_completion = true;
        redraw = false;

        XFlush(dis);
    }
}


