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
    bool is_horizontal;
    struct plot_object_parameter *params;
    int param_count;
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
    for (int i = 0; i < plotter->state_var_count; i++) {
        plotter->update_and_construct.values[i] = plotter->state_vars[i];
    }
    if (moved_plot_object >= 0
        && moved_plot_object < plotter->plot_object_count
        && plotter->plot_objects[moved_plot_object].type == PLOT_FREE_POINT)
    {
        struct plot_object *it = &plotter->plot_objects[moved_plot_object];
        int xi = it->params[0].val_index;
        int yi = it->params[1].val_index;
        plotter->update_and_construct.values[xi] = move_x;
        plotter->update_and_construct.values[yi] = move_y;
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
            else param->prev_val = output[param->val_index];
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

void draw_horizontal_line(ImageView *out, struct camera *camera, long y) {
    char *data = out->data;
    int stride = out->stride;

    int j = out->height / 2 - (y - camera->centre_y) / camera->zoom;
    if (j >= 0 && j < out->height) {
        unsigned *target_row = (unsigned*)&data[j * stride];
        for (int i = 0; i < out->width; i++) target_row[i] = 0x888888;
    }
}

void draw_vertical_line(ImageView *out, struct camera *camera, long x) {
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

void draw_point(ImageView *out, struct camera *camera, long colour, int x, int y) {
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

void draw_points(ImageView *out, struct camera *camera, long colour, int count, long *xs, long *ys) {
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

void draw_plotter_objects(
    ImageView *out,
    struct camera *camera,
    struct plotter_state *plotter
) {
    fill_rectangle(out, 0, 0, out->width, out->height, (RGBA){0, 0, 0});

    int64 *values;
    {
        struct function *f = &plotter->update_and_construct;
        int value_count = f->arg_count + f->intermediates_count;
        values = &f->values[value_count - f->result_count];
    }

    for (int i = 0; i < plotter->plot_object_count; i++) {
        struct plot_object *it = &plotter->plot_objects[i];

        switch (it->type) {
        case PLOT_FREE_POINT:
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
            if (it->is_horizontal) draw_horizontal_line(out, camera, c);
            else draw_vertical_line(out, camera, c);
            break;
          }
        case PLOT_FUNCTION:
            break;
        }
    }
}

#endif
