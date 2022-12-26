#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "window.h"
#include "entrypoint.h"

#include "calculation.h"
#include "plotter.h"
#include "deserialize.h"

struct plotter_state make_demo_plotter(void) {
    struct plotter_state plotter = {0};
    plotter.state_var_count = 2;
    int64 *state = malloc(2 * sizeof(int64));
    state[0] = 0;
    state[1] = 100;
    plotter.state_vars = state;
    {
        struct function_builder builder = create_function_builder(2);
        plotter.update_and_construct = build_function(&builder, 2);
    }

    plotter.plot_object_count = 4;
    struct plot_object *plot_objects =
        malloc(plotter.plot_object_count * sizeof(struct plot_object));

    plot_objects[0].type = PLOT_FUNCTION;
    plot_objects[0].is_vertical = false;
    plot_objects[0].params = malloc(4 * sizeof(struct plot_object_parameter));
    plot_objects[0].param_count = 4;
    plot_objects[0].params[0].is_variable = false;
    plot_objects[0].params[0].constant_val = -1000;
    plot_objects[0].params[1].is_variable = false;
    plot_objects[0].params[1].constant_val = 1000;
    plot_objects[0].params[2].is_variable = true;
    plot_objects[0].params[2].val_index = 0;
    plot_objects[0].params[3].is_variable = true;
    plot_objects[0].params[3].val_index = 1;
    {
        struct function_builder builder = create_function_builder(5);
        struct instruction instr;

        /* v5 = x * y1 */
        instr.op = OP_MUL;
        instr.binary.arg1 = 0;
        instr.binary.arg2 = 4;
        function_builder_push(&builder, &instr);

        /* y = v5 / x1
             = x * y1 / x1
            ~= m*x          */
        instr.op = OP_DIV;
        instr.binary.arg1 = 5;
        instr.binary.arg2 = 3;
        function_builder_push(&builder, &instr);

        plot_objects[0].function = build_function(&builder, 1);
    }

    plot_objects[1].type = PLOT_STATIC_POINT;
    plot_objects[1].params = malloc(2 * sizeof(struct plot_object_parameter));
    plot_objects[1].param_count = 2;
    plot_objects[1].params[0].is_variable = false;
    plot_objects[1].params[0].constant_val = 0;
    plot_objects[1].params[1].is_variable = true;
    plot_objects[1].params[1].val_index = 1;

    plot_objects[2].type = PLOT_STATIC_POINT;
    plot_objects[2].params = malloc(2 * sizeof(struct plot_object_parameter));
    plot_objects[2].param_count = 2;
    plot_objects[2].params[0].is_variable = true;
    plot_objects[2].params[0].val_index = 0;
    plot_objects[2].params[1].is_variable = false;
    plot_objects[2].params[1].constant_val = 0;

    plot_objects[3].type = PLOT_FREE_POINT;
    plot_objects[3].params = malloc(2 * sizeof(struct plot_object_parameter));
    plot_objects[3].param_count = 2;
    plot_objects[3].params[0].is_variable = true;
    plot_objects[3].params[0].val_index = 0;
    plot_objects[3].params[1].is_variable = true;
    plot_objects[3].params[1].val_index = 1;

    plotter.plot_objects = plot_objects;

    return plotter;
}

void make_test_deserialize_input(void) {
    byte data[] = {
        DE_COMMAND_NEW_CONSTRUCT, 2, 50, 20, 8,
            OP_SUB, 0, 1,
            OP_ADD, 0, 1,
            OP_GREATER | OP_IMM2, 0, 0,
            OP_NEG, 0,
            OP_SELECT, 0, 5, 4,
            OP_NEG, 6,
            OP_MOV, 0,
            OP_MOV, 1,
        DE_COMMAND_ADD_HORIZONTAL_AXIS, 0, 0,
        DE_COMMAND_ADD_VERTICAL_AXIS, 0, 0,
        DE_COMMAND_ADD_STATIC_POINT, 1, 2, 1, 3,
        DE_COMMAND_ADD_FREE_POINT, 1, 0, 1, 1,
        DE_COMMAND_ADD_HORIZONTAL_CURVE, 2, 1, 7, 1, 6, 2,
            OP_MUL, 0, 0,
            OP_DIV | OP_IMM2, 3, 128, 1
    };

    if (buffer_cap > 0) free(buffer);
    buffer = malloc(sizeof(data));
    memcpy(buffer, data, sizeof(data));
    buffer_start = 0;
    buffer_cap = sizeof(data);
    buffer_count = sizeof(data);
}

int entry_point(int argc, char **argv) {
    struct window win;
    create_window(&win, "Foundational Plotting Environment", false);

    struct camera camera = {0};
    camera.zoom = 1;

    struct plotter_state plotter = make_demo_plotter();
    struct deserialize_state deserialize_state = {0};

    int selected_object = -1;

    long frame = 0;
    while (true) {
        struct window_event event;
        if (get_event(&win, &event, false)) {
            if (event.type == WINDOW_CLOSE) {
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

        try_deserialize_input(&plotter, &deserialize_state);

        int64 pos_x = camera.centre_x + (win.mouse_x - win.width / 2) * camera.zoom;
        int64 pos_y = camera.centre_y - (win.mouse_y - win.height / 2) * camera.zoom;
        update_plotter_state(&plotter, selected_object, pos_x, pos_y);
        draw_plotter_objects(&win.back_buf, &camera, &plotter);

        flush_window(&win);

        frame++;
    }

    return 0;
}
