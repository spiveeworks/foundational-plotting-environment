-module(fpe_examples).

-export([movable_interval/0, movable_interval/1]).

interval() ->
    {F0, [X, X1, _X2, Y1, Width, Height]} = fpe:create_function(6),
    {F1, Dx} = fpe:sub(F0, X, X1),
    {F2, DyDx} = fpe:mul(F1, Dx, Height),
    {F3, SemiWidth} = fpe:divide(F2, Width, 2),
    {F4, DyDxPlus} = fpe:add(F3, DyDx, SemiWidth),
    {F5, Dy} = fpe:divide(F4, DyDxPlus, Width),
    {F6, Y} = fpe:add(F5, Y1, Dy),
    {F6, Y}.

movable_interval() ->
    I = fpe:start_instance("main.exe"),
    movable_interval(I).

movable_interval(I) ->
    {F0, [X1, Y1, X2, Y2]} = fpe:create_function(4),
    {F1, Cmp} = fpe:less(F0, X1, X2),
    {F2, LeftX} = fpe:select(F1, X1, X2, Cmp),
    {F3, RightX} = fpe:select(F2, X2, X1, Cmp),
    {F4, LeftY} = fpe:select(F3, Y1, Y2, Cmp),
    {F5, RightY} = fpe:select(F4, Y2, Y1, Cmp),
    {F6, Width} = fpe:sub(F5, RightX, LeftX),
    {F7, Height} = fpe:sub(F6, RightY, LeftY),
    Construction = F7,

    {IntervalFunction, IntervalOutput} = interval(),

    {ok, NewI} = fpe:reset_construction(I, [-100, 0, 100, 0], Construction,
                                        [X1, Y1, X2, Y2]),
    fpe:add_horizontal_curve(NewI, [LeftX, RightX, LeftY, Width, Height],
                             IntervalFunction, IntervalOutput),
    fpe:add_free_point(NewI, X1, Y1),
    fpe:add_free_point(NewI, X2, Y2).


