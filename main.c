#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

void draw_frame(XImage *back_buf, long t) {
    int width = back_buf->width;
    int height = back_buf->height;

    long y[width];
    char *data = back_buf->data + back_buf->xoffset;
    int stride = back_buf->bytes_per_line;

    long z = t % 512;
    if (z > 256) z = 512 - z;

    for (int j = 0; j < height; j++) {
        long y = j - height / 2;
        unsigned *row = (unsigned*)&data[stride * j];
        for (int i = 0; i < width; i++) {
            long x = i - width / 2;

            long r = z + x;
            long g = z - y * 221 / 256 - x / 2;
            long b = z + y * 221 / 256 - x / 2;

            if (r < 0 || r >= 256 || g < 0 || g >= 256 || b < 0 || b > 256) {
                row[i] = 0;
            } else {
                row[i] = r << 16 | g << 8 | b;
            }
        }
    }
}

int main(int argc, char **argv) {
    bool benchmark = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-benchmark") == 0) {
            if (benchmark) fprintf(stderr, "Warning: \"-benchmark\" option appeared more than once.\n");
            benchmark = true;
        } else {
            fprintf(stderr, "Warning: \"%s\" is not a known command line argument.\n", argv[i]);
        }
    }

    Display *dis = XOpenDisplay(NULL);
    Window win = XCreateSimpleWindow(dis, DefaultRootWindow(dis), 0, 0,
            600, 600, 0, 0xFFFFFF, 0x000000);

    if (!XShmQueryExtension(dis)) {
        fprintf(stderr, "Error: The shared memory extension is required for this program to run.\n");
        exit(EXIT_FAILURE);
    }

    XSetStandardProperties(dis, win,
        "Colour Rhombohedron",
        "Colour Rhombohedron",
        None, NULL, 0, NULL);

    long mask = StructureNotifyMask | ButtonPressMask | ButtonReleaseMask
        | KeyPressMask | KeyReleaseMask | PointerMotionMask | FocusChangeMask
        | StructureNotifyMask;
    XSelectInput(dis, win, mask);

    GC gc = XCreateGC(dis, win, 0, 0);

    XClearWindow(dis, win);
    XMapRaised(dis, win);

    int window_width = 600;
    int window_height = 600;

    bool back_buf_resize = false;
    XImage *back_buf = NULL;
    XImage *unscaled_buf = NULL;
    XShmSegmentInfo shm_info;

    bool waiting_for_shm_completion = false;
    const int SHM_COMPLETION = XShmGetEventBase (dis) + ShmCompletion;

    long t = 0;
    bool play = true;

    struct timespec frame_end;
    clock_gettime(CLOCK_MONOTONIC_RAW, &frame_end);

    while (true) {
        XEvent event;
        XNextEvent(dis, &event);

        if (event.type == ConfigureNotify) {
            back_buf_resize = true;
        } else if (event.type == SHM_COMPLETION) {
            waiting_for_shm_completion = false;
        } else if (event.type == KeyPress) {
            KeySym key;
            char text[64];

            if (XLookupString(&event.xkey, text, 255, &key, 0) == 1) {
                if (key == XK_Escape) exit(EXIT_SUCCESS);
                if (text[0] == ' ') play = !play;
            }
        } else if (event.type == DestroyNotify) {
            exit(EXIT_SUCCESS);
        }

        if (!back_buf && !back_buf_resize) continue;
        if (waiting_for_shm_completion) continue;
        if (XEventsQueued(dis, QueuedAlready) > 0) continue;

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

        draw_frame(back_buf, t);

        XShmPutImage (dis, win, gc, back_buf, 0, 0, 0, 0, window_width, window_height, true);
        waiting_for_shm_completion = true;

        XFlush(dis);

        if (play) t += 1;

        long second = 1000000000;
        long frame = second / 60;

        struct timespec now;
        clock_gettime(CLOCK_MONOTONIC_RAW, &now);

        long elapsed = (now.tv_sec - frame_end.tv_sec) * second
            + (now.tv_nsec - frame_end.tv_nsec);

        if (benchmark) printf("Took %dms.\n", elapsed / 1000000);

        if (elapsed < frame) {
            struct timespec remaining = {0, frame - elapsed};
            nanosleep(&remaining, NULL);
        }

        clock_gettime(CLOCK_MONOTONIC_RAW, &frame_end);
    }
}

