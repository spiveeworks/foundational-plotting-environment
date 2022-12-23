#ifndef FPE_PLOTTER_H
#define FPE_PLOTTER_H

#include "image.h"

#include "calculation.h"

enum plot_object_type {
    PLOT_FREE_POINT,
    PLOT_STATIC_POINT,
    PLOT_AXIS,
    PLOT_FUNCTION,
};

struct plot_object_parameter {
    bool is_constant;
    union {
        int64 constant_val;
        int val_index;
    };
    int64 prev_val;
};

struct plot_object {
    enum plot_object_type type;
    bool is_vertical;
    struct plot_object_parameter *params;
    int param_count;

    struct function function;
};

struct plotter_state {
    int state_var_count;
    int64 *state_vars;
    struct function update_and_construct;

    struct plot_object *plot_objects;
    int plot_object_count;
};

void update_plotter_state(
    struct plotter_state *plotter,
    int moved_plot_object,
    int64 move_x,
    int64 move_y
) {
    if (plotter->state_var_count != plotter->update_and_construct.arg_count) {
        exit(EXIT_FAILURE);
    }
    int64 *values = plotter->update_and_construct.values;
    for (int i = 0; i < plotter->state_var_count; i++) {
        values[i] = plotter->state_vars[i];
    }
    if (moved_plot_object >= 0
        && moved_plot_object < plotter->plot_object_count
        && plotter->plot_objects[moved_plot_object].type == PLOT_FREE_POINT)
    {
        struct plot_object *it = &plotter->plot_objects[moved_plot_object];
        int xi = it->params[0].val_index;
        int yi = it->params[1].val_index;
        values[xi] = move_x;
        values[yi] = move_y;
    }

    int64 *output = calculate_function(&plotter->update_and_construct);
    for (int i = 0; i < plotter->state_var_count; i++) {
        plotter->state_vars[i] = output[i];
    }

    for (int i = 0; i < plotter->plot_object_count; i++) {
        struct plot_object *it = &plotter->plot_objects[i];
        for (int j = 0; j < it->param_count; j++) {
            struct plot_object_parameter *param = &it->params[j];
            if (param->is_constant) param->prev_val = param->constant_val;
            else param->prev_val = values[param->val_index];
        }
    }
}

/* Actual drawing */

void fill_rectangle(
    ImageView *out,
    int left,
    int top,
    int right,
    int bottom,
    RGBA colour_channels
) {
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
        unsigned *target_row = (unsigned*)&out->data[out->stride * j];
        for (int i = left; i <= right; i++) {
            target_row[i] = word;
        }
    }
}

struct camera {
    int64 centre_x;
    int64 centre_y;
    int64 zoom;
};

void draw_horizontal_line(ImageView *out, struct camera *camera, int64 y) {
    char *data = out->data;
    int stride = out->stride;

    int j = out->height / 2 - (y - camera->centre_y) / camera->zoom;
    if (j >= 0 && j < out->height) {
        unsigned *target_row = (unsigned*)&data[j * stride];
        for (int i = 0; i < out->width; i++) target_row[i] = 0x888888;
    }
}

void draw_vertical_line(ImageView *out, struct camera *camera, int64 x) {
    char *data = out->data;
    int stride = out->stride;

    int i = out->width / 2 + (x - camera->centre_x) / camera->zoom;
    if (i >= 0 && i < out->width) {
        for (int j = 0; j < out->height; j++) {
            unsigned *row = (unsigned*)data;
            row[i] = 0x888888;
            data += stride;
        }
    }
}

void draw_point(ImageView *out, struct camera *camera, uint colour, int x, int y) {
    int i =  out->width / 2 + (x - camera->centre_x) / camera->zoom;
    int j = out->height / 2 - (y - camera->centre_y) / camera->zoom;

    if (i < 1) return;
    if (i >= out->width - 1) return;
    if (j < 1) return;
    if (j >= out->height - 1) return;

    unsigned *target_row = (unsigned*)&out->data[out->stride * (j-1)];
    target_row[i] = colour;
    target_row = (unsigned*)((char*)target_row + out->stride);
    target_row[i-1] = colour;
    target_row[i] = colour;
    target_row[i+1] = colour;
    target_row = (unsigned*)((char*)target_row + out->stride);
    target_row[i] = colour;
}

void draw_points(ImageView *out, struct camera *camera, uint colour, int count, int64 *xs, int64 *ys) {
    char *data = out->data;
    int stride = out->stride;

    for (int ind = 0; ind < count; ind++) {
        int i =  out->width / 2 + (xs[ind] - camera->centre_x) / camera->zoom;
        int j = out->height / 2 - (ys[ind] - camera->centre_y) / camera->zoom;

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

void draw_curve(
    ImageView *out,
    struct camera *camera,
    struct function *f,
    struct plot_object_parameter *params,
    int param_count
) {
    if (f->arg_count != param_count + 1) {
        exit(EXIT_FAILURE);
    }

    int64 a = params[0].prev_val;
    int64 b = params[1].prev_val;
    int min =  out->width / 2 + (a - camera->centre_x) / camera->zoom;
    int max =  out->width / 2 + (b - camera->centre_x) / camera->zoom;

    if (min < 1) min = 1;
    if (max >= out->width - 1) max = out->width - 2;

    int count = max - min + 1;

    if (count <= 0) return;

    for (int i = 0; i < param_count; i++) {
        f->values[i + 1] = params[i].prev_val;
    }

    for (int sx = min; sx <= max; sx++) {
        int64 x = camera->centre_x + (sx - out->width / 2) * camera->zoom;

        f->values[0] = x;
        /* This one line hides most of the cost of this loop. We try to make
           the rest as lean as we can, but ultimately we have to trust the
           compiler to do good things with calculate_function's hot loop.
           Function plotting is embarassingly parallel though, so we can make
           this SIMD and/or parallel when necessary. */
        int64 *vals = calculate_function(f);

        /* inlined draw_point procedure */
        /* Calculate sy as an int64 so that LLONG_MAX doesn't get cast to -1 */
        int64 sy = out->height / 2 - (vals[0] - camera->centre_y) / camera->zoom;
        sy = vals[0] - camera->centre_y;
        sy = sy / camera->zoom;
        sy = out->height / 2 - sy;

        if (sy < 1) continue;
        if (sy >= out->height - 1) continue;

        unsigned *target_row = (unsigned*)&out->data[out->stride * (sy-1)];
        target_row[sx] = 0xFFFFFF;
        target_row = (unsigned*)((char*)target_row + out->stride);
        target_row[sx-1] = 0xFFFFFF;
        target_row[sx] = 0xFFFFFF;
        target_row[sx+1] = 0xFFFFFF;
        target_row = (unsigned*)((char*)target_row + out->stride);
        target_row[sx] = 0xFFFFFF;
    }
}

void draw_plotter_objects(
    ImageView *out,
    struct camera *camera,
    struct plotter_state *plotter
) {
    fill_rectangle(out, 0, 0, out->width, out->height, (RGBA){0, 0, 0});

    for (int i = 0; i < plotter->plot_object_count; i++) {
        struct plot_object *it = &plotter->plot_objects[i];

        switch (it->type) {
        case PLOT_FREE_POINT:
          {
            int x = it->params[0].prev_val;
            int y = it->params[1].prev_val;
            draw_point(out, camera, 0x00CCFF, x, y);
            break;
          }
        case PLOT_STATIC_POINT:
          {
            int x = it->params[0].prev_val;
            int y = it->params[1].prev_val;
            draw_point(out, camera, 0xFFFFFF, x, y);
            break;
          }
        case PLOT_AXIS:
          {
            int c = it->params[0].prev_val;
            if (it->is_vertical) draw_vertical_line(out, camera, c);
            else draw_horizontal_line(out, camera, c);
            break;
          }
        case PLOT_FUNCTION:
            draw_curve(
                out,
                camera,
                &it->function,
                it->params,
                it->param_count
            );
            break;
        }
    }
}

#endif
