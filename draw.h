#ifndef FPE_DRAW_H
#define FPE_DRAW_H

#include <ft2build.h>
#include FT_FREETYPE_H

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
        "DejaVuSans.ttf",
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

#endif
