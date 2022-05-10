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

#define STBI_NO_SIMD
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#include <ft2build.h>
#include FT_FREETYPE_H

Display *dis;
Window win;
GC gc;

FT_Library  library;
FT_Face     face;

typedef struct {
    unsigned char b;
    unsigned char g;
    unsigned char r;
    unsigned char a;
} RGBA;

typedef struct str {
    unsigned char *data;
    size_t size;
} str;

#define GLYPH_COUNT 256
struct Glyph {
    int width;
    int height;
    int bearingx;
    int bearingy; /* ascent in XLib terms. */
    int advance;
    /* Glyphs have a single value/opacity channel, which we then blend onto the
     * screen ourselves. */
    unsigned char *data;
};

struct Glyph glyphs[GLYPH_COUNT];
typedef struct Glyph *Glyph;

void init_text(void) {
    FT_Error error = FT_Init_FreeType(&library);
    if (error) {
        fprintf(stderr, "ERROR: failed to initialise FreeType library\n");
        exit(1);
    }

    error = FT_New_Face(
        library,
        "data/DejaVuSans.ttf",
         0,
         &face
    );
    if (error == FT_Err_Unknown_File_Format) {
        fprintf(stderr, "ERROR: unknown font format\n");
        exit(1);
    } else if (error) {
        fprintf(stderr, "ERROR: failed to load font\n");
        exit(1);
    }

    error = FT_Set_Pixel_Sizes(face, 0, 16);
    if (error) {
        fprintf(stderr, "ERROR: failed to set char size\n");
        exit(1);
    }

    for (int i = 0; i < GLYPH_COUNT; i++) {
        error = FT_Load_Char(face, i, 0);
        if (error)
            continue;  /* ignore errors */

        FT_Glyph_Metrics *metrics = &face->glyph->metrics;
        int width = metrics->width >> 6;
        int height = metrics->height >> 6;
        glyphs[i].width = width;
        glyphs[i].height = height;
        glyphs[i].width = metrics->width >> 6;
        glyphs[i].height = metrics->height >> 6;
        glyphs[i].bearingx = metrics->horiBearingX >> 6;
        glyphs[i].bearingy = metrics->horiBearingY >> 6;
        glyphs[i].advance = metrics->horiAdvance >> 6;

        glyphs[i].data = malloc(width * height);

        /* load glyph image into the slot (erase previous one) */
        error = FT_Load_Char(face, i, FT_LOAD_RENDER);
        if (error)
            continue;  /* ignore errors */

        /* save a copy of it */
        memcpy(glyphs[i].data, face->glyph->bitmap.buffer, width * height);
    }
}

void string_dimensions(
    str text,
    int *ascent_out,
    int *descent_out,
    int *width_out
) {
    int ascent = 0;
    int descent = 0;
    int width = 0;
    for (int i = 0; i < text.size; i++) {
        unsigned char c = text.data[i];
        int this_ascent = glyphs[c].bearingy;
        int this_descent = glyphs[c].height - this_ascent;
        if (this_ascent > ascent) ascent = this_ascent;
        if (this_descent > descent) descent = this_descent;
        width += glyphs[c].advance;
    }
    if (ascent_out) *ascent_out = ascent;
    if (descent_out) *descent_out = descent;
    if (width_out) *width_out = width;
}

void draw_string(XImage *out, int x, int y, str text) {
    for (int ind = 0; ind < text.size; ind++) {
        unsigned char c = text.data[ind];
        int width = glyphs[c].width;
        int height = glyphs[c].height;

        char *out_data = out->data;
        int bytes_per_line = out->bytes_per_line;

        int corner_x = x + glyphs[c].bearingx;
        int corner_y = y - glyphs[c].bearingy;
        int start_i = corner_x < 0 ? -corner_x : 0;
        int start_j = corner_y < 0 ? -corner_y : 0;

        int end_i = corner_x + width > out->width ? out->width - corner_x : width;
        int end_j = corner_y + height > out->height ? out->height - corner_y : height;

        x += glyphs[c].advance;

        if (end_i <= start_i) continue;
        for (int j = start_j; j < end_j; j++) {
            unsigned char *source_row = &glyphs[c].data[j * width];
            RGBA *target_row = (RGBA*)&out_data[bytes_per_line * (corner_y + j)];
            target_row += corner_x;
            for (int i = start_i; i < end_i; i++) {
                unsigned short a = source_row[i];
                a = a + 50 - (a + 50) % 51; /* round up to multiple of 255/5 */
                unsigned short b = 255 - a;
                RGBA pix = target_row[i];
                pix.r = a + b * pix.r / 256;
                pix.g = a + b * pix.g / 256;
                pix.b = a + b * pix.b / 256;
                target_row[i] = pix;
            }
        }
    }
}

void fill_rectangle(XImage *out, int left, int top, int right, int bottom, RGBA colour_channels) {
    if (left < 0) left = 0;
    if (right >= out->width) right = out->width - 1;
    if (top < 0) top = 0;
    if (bottom >= out->height) bottom = out->height - 1;

    if (left > right) return;

    union {
        RGBA channels;
        unsigned word;
    } colour;
    colour.channels = colour_channels;
    unsigned word = colour.word;

    for (int j = top; j <= bottom; j++) {
        unsigned *target_row = (unsigned*)&out->data[out->bytes_per_line * j];
        for (int i = left; i <= right; i++) {
            target_row[i] = word;
        }
    }
}


int main(int argc, char **argv) {
    bool key_down[256] = {};

    {
        dis = XOpenDisplay(NULL);
        int screen = DefaultScreen(dis);

        win = XCreateSimpleWindow(dis, DefaultRootWindow(dis), 0, 0,
            600, 600, 0, 0xFFFFFF, 0x000000);
    }

    if (!XShmQueryExtension(dis)) {
        fprintf(stderr, "Error: The shared memory extension is required for this program to run.\n");
        exit(EXIT_FAILURE);
    }

    XSetStandardProperties(dis, win,
        "Foundational Plotting Envrionment",
        "Foundational Plotting Environment",
        None, NULL, 0, NULL);

    long mask = StructureNotifyMask | ButtonPressMask | ButtonReleaseMask
        | KeyPressMask | KeyReleaseMask | PointerMotionMask | FocusChangeMask
        | StructureNotifyMask;
    XSelectInput(dis, win, mask);

    gc = XCreateGC(dis, win, 0, 0);

    XClearWindow(dis, win);
    XMapRaised(dis, win);

    int window_width = 600;
    int window_height = 600;

    bool back_buf_resize = false;
    XImage *back_buf = NULL;
    XImage *unscaled_buf = NULL;
    XShmSegmentInfo shm_info;

    /* We could ask X for the initial mouse position. */
    int mouse_x = 0;
    int mouse_y = 0;

    bool waiting_for_shm_completion = false;
    const int SHM_COMPLETION = XShmGetEventBase (dis) + ShmCompletion;

    bool redraw = false;

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
            if (button == 2) {
                /* panning = true; */
            }
        } else if (event.type == ButtonRelease) {
            if (event.xbutton.button == 2) {
                /* panning = false; */
            }
        } else if (event.type == MotionNotify) {
            mouse_x = event.xmotion.x;
            mouse_y = event.xmotion.y;
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

        int width = back_buf->width;
        int height = back_buf->height;

        fill_rectangle(back_buf, 0, 0, width, height, (RGBA){0, 0, 0});

        long y[width];
        for (int i = 0; i < width; i++) {
            long x = i - width / 2;
            x *= 1024;
            x /= 30;
            y[i] = x * x / 1024;
        }
        int j1 = height / 2 - y[0] / 1024;
        int j2 = height / 2 - y[1] / 1024;
        char *data = back_buf->data + back_buf->xoffset;
        int stride = back_buf->bytes_per_line;
        for (int i = 1; i < width - 10; i++) {
            int j0 = j1;
            j1 = j2;
            j2 = height / 2 - y[i + 1] / 1024;

            int min = (j0 + j1) / 2;
            int max = (j1 + j2) / 2;
            if (min > max) {
                min = (j1 + j2) / 2;
                max = (j0 + j1) / 2;
            }
            min--;
            max++;
            if (min < 0) min = 0;
            if (max >= height) max = height - 1;

            for (int j = min; j <= max; j++) {
                unsigned *target_row = (unsigned*)&data[stride * j];
                target_row[i] = 0xFFFFFF;
            }
        }

        XShmPutImage (dis, win, gc, back_buf, 0, 0, 0, 0, window_width, window_height, true);
        waiting_for_shm_completion = true;
        redraw = false;

        XFlush(dis);
    }
}


