-module(fpe_examples).

-export([movable_interval/0, movable_interval/1]).
-export([square_root_demo/0, square_root_demo/1]).
-export([circle_demo/0, circle_demo/1]).

interval() ->
    Def = {function,
     [x, x1, x2, y1, width, height, bias],
     [{dx, {sub, x, x1}},
      % Basically x * (y2 - y1)/(x2 - x1), but we do a bit more work to make
      % the integer approximation round at the right places.
      {dy, {divide, {add, {mul, dx, height}, bias}, width}},
      {y, {add, y1, dy}}],
     y},
    fpe_func:flatten_function(Def).

movable_interval() ->
    I = fpe:start_instance("fpe"),
    movable_interval(I).

movable_interval(OldI) ->
    Def = {multi_function,
     [x1, y1, x2, y2],
     [{cmp, {less, x1, x2}},
      {leftx, {select, x1, x2, cmp}},
      {rightx, {select, x2, x1, cmp}},
      {lefty, {select, y1, y2, cmp}},
      {righty, {select, y2, y1, cmp}},
      {width, {sub, rightx, leftx}},
      {height, {sub, righty, lefty}},
      {semi_width, {divide, width, 2}},
      {bias, {select, semi_width, {neg, semi_width}, {greater, height, 0}}}],
     [leftx, rightx, lefty, width, height, bias, x1, x2, y1, y2]},

    {Construction, Args} = fpe_func:flatten_function(Def),

    [LeftX, RightX, LeftY, Width, Height, Bias, X1, X2, Y1, Y2] = Args,

    {IntervalCalculation, [IntervalOutput]} = interval(),

    {ok, I} = fpe:reset_construction(OldI, [-100, 0, 100, 0], Construction,
                                        [X1, Y1, X2, Y2]),
    fpe:add_horizontal_curve(I, {LeftX, RightX},
                             [LeftY, Width, Height, Bias],
                             IntervalCalculation, IntervalOutput),
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
    {Construction, []} = fpe:create_calculation(0),
    {ok, I} = fpe:reset_construction(OldI, [], Construction, []),

    fpe:add_horizontal_line(I, 0),
    fpe:add_vertical_line(I, 0),

    {SqrtBase, [XRaw, _LeftBound, _RightBound]} = fpe:create_calculation(3),
    {Rescaled, X} = fpe:mul(SqrtBase, XRaw, 100),
    {Sqrt, SqrtOut} = integer_square_root(Rescaled, X),
    fpe:add_horizontal_curve(I, {0, 1000}, [], Sqrt, SqrtOut).

semicircle() ->
    {F0, [X, _LeftBound, _RightBound, Quadrance]} = fpe:create_calculation(4),
    {F1, YSquared} = fpe:expr(F0, {sub, Quadrance, {mul, X, X}}),
    {F2, YRaw} = integer_square_root(F1, YSquared),
    {_F3, _Y} = fpe:expr(F2, {select, YRaw, 16#8000000000000000,
                            {greater_or_equal, YSquared, 0}}).

circle_demo() ->
    I = fpe:start_instance("fpe"),
    circle_demo(I).

circle_demo(OldI) ->
    {F0, [X1, Y1]} = fpe:create_calculation(2),
    {Construction, Quadrance} = fpe:expr(F0, {add, {mul, X1, X1}, {mul, Y1, Y1}}),
    {ok, I} = fpe:reset_construction(OldI, [100, 100], Construction, [X1, Y1]),

    fpe:add_horizontal_line(I, 0),
    fpe:add_vertical_line(I, 0),

    {PosCurve, PosCurveOut} = semicircle(),
    fpe:add_horizontal_curve(I, {-1000, 1000}, [Quadrance], PosCurve, PosCurveOut),
    {NegCurve, NegCurveOut} = fpe:neg(PosCurve, PosCurveOut),
    fpe:add_horizontal_curve(I, {-1000, 1000}, [Quadrance], NegCurve, NegCurveOut),

    fpe:add_free_point(I, X1, Y1).

