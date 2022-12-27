-module(fpe_examples).

-export([movable_interval/0, movable_interval/1]).
-export([square_root_demo/0, square_root_demo/1]).
-export([circle_demo/0, circle_demo/1]).

interval() ->
    {F0, [X, X1, _X2, Y1, Width, Height, Bias]} = fpe:create_function(7),
    {F1, Dx} = fpe:sub(F0, X, X1),
    % Basically x * (y2 - y1)/(x2 - x1), but we do a bit more work to make the
    % integer approximation round at the right places.
    {F2, Dy} = fpe:expr(F1, {divide, {add, {mul, Dx, Height}, Bias}, Width}),
    {_F3, _Y} = fpe:add(F2, Y1, Dy).

movable_interval() ->
    I = fpe:start_instance("fpe"),
    movable_interval(I).

movable_interval(OldI) ->
    {F0, [X1, Y1, X2, Y2]} = fpe:create_function(4),
    {F1, Cmp} = fpe:less(F0, X1, X2),
    {F2, LeftX} = fpe:select(F1, X1, X2, Cmp),
    {F3, RightX} = fpe:select(F2, X2, X1, Cmp),
    {F4, LeftY} = fpe:select(F3, Y1, Y2, Cmp),
    {F5, RightY} = fpe:select(F4, Y2, Y1, Cmp),
    {F6, Width} = fpe:sub(F5, RightX, LeftX),
    {F7, Height} = fpe:sub(F6, RightY, LeftY),
    {F8, SemiWidth} = fpe:divide(F7, Width, 2),
    {F9, Bias} = fpe:expr(F8, {select,
                               SemiWidth,
                               {neg, SemiWidth},
                               {greater, Height, 0}}),
    Construction = F9,

    {IntervalFunction, IntervalOutput} = interval(),

    {ok, I} = fpe:reset_construction(OldI, [-100, 0, 100, 0], Construction,
                                        [X1, Y1, X2, Y2]),
    fpe:add_horizontal_curve(I, {LeftX, RightX},
                             [LeftY, Width, Height, Bias],
                             IntervalFunction, IntervalOutput),
    fpe:add_free_point(I, X1, Y1),
    fpe:add_free_point(I, X2, Y2).

bisect(F0, IterationsLeft, X, Delta, G, Y) when IterationsLeft > 0 ->
    {F1, XPlus} = fpe:add(F0, X, Delta),
    {F2, Result} = G(F1, XPlus),
    {F3, XNew} = fpe:expr(F2, {select, XPlus, X, {less_or_equal, Result, Y}}),
    bisect(F3, IterationsLeft - 1, XNew, Delta div 2, G, Y);
bisect(F, 0, X, _, _, _) ->
    {F, X}.

square(F, X) ->
    fpe:mul(F, X, X).

integer_square_root(F0, Y) ->
    {F1, Log2} = fpe:integer_log(F0, Y),
    % Log base 4 and round off, so log(2) = log( 4) = log( 7) = 1,
    %                          and log(8) = log(16) = log(31) = 2.
    {F2, Log4} = fpe:divide(F1, Log2, 2),
    % Multiply or divide by 4 until it is in the range [0.5P, 2P], for some
    % largeish P
    PivotSize = 32,
    % Pivot = 1 bsl PivotSize,
    {F3, Shift} = fpe:expr(F2, {sub, PivotSize, {mul, Log4, 2}}),
    {F4, Shifted} = fpe:shift_left(F3, Y, Shift),
    % Bisect to find a square root of Shifted
    X1 = 1 bsl (PivotSize div 2),
    Delta1 = X1 div 2,
    {F5, X} = bisect(F4, 10, X1, Delta1, fun square/2, Shifted),
    % For each 4 we multiplied before, divide by 2 to compensate.
    fpe:expr(F5, {shift_right, X, {sub, PivotSize div 2, Log4}}).

square_root_demo() ->
    I = fpe:start_instance("fpe"),
    square_root_demo(I).

square_root_demo(OldI) ->
    {Construction, []} = fpe:create_function(0),
    {ok, I} = fpe:reset_construction(OldI, [], Construction, []),

    fpe:add_horizontal_line(I, 0),
    fpe:add_vertical_line(I, 0),

    {SqrtBase, [XRaw, _LeftBound, _RightBound]} = fpe:create_function(3),
    {Rescaled, X} = fpe:mul(SqrtBase, XRaw, 100),
    {Sqrt, SqrtOut} = integer_square_root(Rescaled, X),
    fpe:add_horizontal_curve(I, {0, 1000}, [], Sqrt, SqrtOut).

semicircle() ->
    {F0, [X, _LeftBound, _RightBound, Quadrance]} = fpe:create_function(4),
    {F1, YSquared} = fpe:expr(F0, {sub, Quadrance, {mul, X, X}}),
    {F2, YRaw} = integer_square_root(F1, YSquared),
    {_F3, _Y} = fpe:expr(F2, {select, YRaw, 16#8000000000000000,
                            {greater_or_equal, YSquared, 0}}).

circle_demo() ->
    I = fpe:start_instance("fpe"),
    circle_demo(I).

circle_demo(OldI) ->
    {F0, [X1, Y1]} = fpe:create_function(2),
    {Construction, Quadrance} = fpe:expr(F0, {add, {mul, X1, X1}, {mul, Y1, Y1}}),
    {ok, I} = fpe:reset_construction(OldI, [100, 100], Construction, [X1, Y1]),

    fpe:add_horizontal_line(I, 0),
    fpe:add_vertical_line(I, 0),

    {PosCurve, PosCurveOut} = semicircle(),
    fpe:add_horizontal_curve(I, {-1000, 1000}, [Quadrance], PosCurve, PosCurveOut),
    {NegCurve, NegCurveOut} = fpe:neg(PosCurve, PosCurveOut),
    fpe:add_horizontal_curve(I, {-1000, 1000}, [Quadrance], NegCurve, NegCurveOut),

    fpe:add_free_point(I, X1, Y1).

