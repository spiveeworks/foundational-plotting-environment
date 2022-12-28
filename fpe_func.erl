-module(fpe_func).

-export([one_liner/2]).
-export([flatten_function/1, flatten_one_liner/2]).

-type name() :: atom() | string().

-spec name_to_string(name()) -> string().

name_to_string(Name) when is_atom(Name) ->
    atom_to_list(Name);
name_to_string(Name) ->
    Name.

-type func() :: {function, [name()], [instruction()], expression()}.
-type multi_func() :: func()
    | {multi_function, [name()], [instruction()], [expression()]}.
-type instruction() :: {name(), expression()}
                     | {[name()], apply, multi_func(), [expression()]}.
-type expression() :: name() | integer() | {fpe:unary_op(), expression()}
    | {fpe:binary_op(), expression(), expression()}
    | {fpe:ternary_op(), expression(), expression(), expression()}
    | {apply, func(), [expression()]}.

-spec one_liner([name()], expression()) -> func().

one_liner(Names, Out) ->
    {function, Names, [], Out}.

-type bindings() :: #{string() => fpe:arg()}.

-spec bindings_from_lists([name()], [fpe:arg()]) ->
    {ok, bindings()} | {error, length_mismatch}.

bindings_from_lists(Names, Args) when length(Names) == length(Args) ->
    NameStrings = lists:map(fun name_to_string/1, Names),
    Bindings = maps:from_list(lists:zip(NameStrings, Args)),
    {ok, Bindings};
bindings_from_lists(_, _) ->
    {error, length_mismatch}.

-spec flatten_expr(fpe:calculation(), bindings(), expression()) ->
    {fpe:calculation(), fpe:arg()}.

flatten_expr(F, _Bindings, X) when is_integer(X) ->
    {F, X};
flatten_expr(F, Bindings, X) when is_atom(X) or is_list(X) ->
    {F, maps:get(name_to_string(X), Bindings)};
flatten_expr(F1, Bindings, {apply, Func, Inputs}) ->
    {F2, Args} = flatten_exprs(F1, Bindings, Inputs),
    {F3, [Output]} = flatten_function(F2, Func, Args),
    {F3, Output};
flatten_expr(F1, Bindings, {Op, AExpr}) ->
    {F2, A} = flatten_expr(F1, Bindings, AExpr),
    fpe:append_instruction(F2, {Op, A});
flatten_expr(F1, Bindings, {Op, AExpr, BExpr}) ->
    {F2, A} = flatten_expr(F1, Bindings, AExpr),
    {F3, B} = flatten_expr(F2, Bindings, BExpr),
    fpe:append_instruction(F3, {Op, A, B});
flatten_expr(F1, Bindings, {Op, AExpr, BExpr, CExpr}) ->
    {F2, A} = flatten_expr(F1, Bindings, AExpr),
    {F3, B} = flatten_expr(F2, Bindings, BExpr),
    {F4, C} = flatten_expr(F3, Bindings, CExpr),
    fpe:append_instruction(F4, {Op, A, B, C}).

-spec flatten_exprs(fpe:calculation(), bindings(), [expression()]) ->
    {fpe:calculation(), [fpe:arg()]}.

flatten_exprs(F1, Bindings, Exprs) ->
    MapFoldFun = fun(Expr, FEach) ->
        % We should probably just change our convention to match Erlang's foldl
        % convention, but eh.
        {FNext, Arg} = flatten_expr(FEach, Bindings, Expr),
        {Arg, FNext}
    end,
    {OutputArgs, F2} = lists:mapfoldl(MapFoldFun, F1, Exprs),
    % Again, a sign we should probably change our convention.
    {F2, OutputArgs}.

% Designed to fold with.
-spec flatten_instruction(instruction(), {fpe:calculation(), bindings()}) ->
    {fpe:calculation(), bindings()}.

flatten_instruction({Names, apply, Func, Inputs}, {F0, Bindings}) ->
    {F1, Args} = flatten_exprs(F0, Bindings, Inputs),
    {F2, Outputs} = flatten_function(F1, Func, Args),
    {ok, NewBindings} = bindings_from_lists(Names, Outputs),
    {F2, maps:merge(Bindings, NewBindings)};
flatten_instruction({Name, Expr}, {F0, Bindings}) ->
    {F1, Arg} = flatten_expr(F0, Bindings, Expr),
    {F1, maps:put(name_to_string(Name), Arg, Bindings)}.

-spec flatten_function(multi_func()) ->
    {fpe:calculation(), [fpe:arg()]}.

flatten_function(Func) ->
    InputCount = length(element(2, Func)),
    {F0, Args} = fpe:create_calculation(InputCount),
    flatten_function(F0, Func, Args).

-spec flatten_function(fpe:calculation(), multi_func(), [fpe:arg()]) ->
    {fpe:calculation(), [fpe:arg()]}.

flatten_function(F, {function, Names, Instructions, Output}, Args) ->
    flatten_function(F, {multi_function, Names, Instructions, [Output]}, Args);
flatten_function(F0, {multi_function, Names, Instructions, Outputs}, Args) ->
    {ok, InitialBindings} = bindings_from_lists(Names, Args),
    {F1, FinalBindings} = lists:foldl(fun flatten_instruction/2,
                                      {F0, InitialBindings},
                                      Instructions),
    flatten_exprs(F1, FinalBindings, Outputs).

-spec flatten_one_liner([name()], expression()) ->
    {fpe:calculation(), [fpe:arg()]}.

flatten_one_liner(Names, Out) ->
    flatten_function(one_liner(Names, Out)).
