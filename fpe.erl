-module(fpe).

-export([start_instance/1, reset_construction/2]).
-export([create_function/1, append_instruction/2]).
-export([function_to_binary/1, append_function_to_binary/2]).

-export([add/3, sub/3, neg/2, shift_left/3, shift_right/3, mul/3,
         divide_modulo/3, divide/3, modulo/3, euclidean_divide_modulo/3,
         euclidean_divide/3, euclidean_modulo/3, integer_log/2, equal/3,
         not_equal/3, less/3, greater/3, less_or_equal/3, greater_or_equal/3,
         logical_and/3, logical_or/3, select/4]).

-type arg() :: {variable, non_neg_integer()} | {constant, integer()}.

-type unary_op() :: negate | integer_log.
-type binary_op() :: add | sub | shift_left | shift_right | mul
    | divide_modulo | divide | modulo
    | euclidean_divide_modulo | euclidean_divide | euclidien_modulo
    | equal | not_equal | less | greater | less_or_equal
    | greater_or_equal | logical_and | logical_or.
-type ternary_op() :: select.
-type operation() :: unary_op() | binary_op() | ternary_op().

-type instruction() :: {unary_op(), arg()} | {binary_op(), arg(), arg()}
    | {ternary_op(), arg(), arg(), arg()}.

-spec opcode(operation()) -> byte().

opcode(add)                     ->  1;
opcode(sub)                     ->  2;
opcode(neg)                     ->  3;
opcode(shift_left)              ->  4;
opcode(shift_right)             ->  5;
opcode(mul)                     ->  6;
opcode(divide_modulo)           ->  7;
opcode(divide)                  ->  8;
opcode(modulo)                  ->  9;
opcode(euclidean_divide_modulo) -> 10;
opcode(euclidean_divide)        -> 11;
opcode(euclidean_modulo)        -> 12;
opcode(integer_log)             -> 13;
opcode(equal)                   -> 14;
opcode(not_equal)               -> 15;
opcode(less)                    -> 16;
opcode(greater)                 -> 17;
opcode(less_or_equal)           -> 18;
opcode(greater_or_equal)        -> 19;
opcode(logical_and)             -> 20;
opcode(logical_or)              -> 21;
opcode(select)                  -> 22.


% Break the int up into twos-complement septets, (7-bit units,) little-endian.
% The most significant bit indicates whether there are more septets remaining.
% This means numbers between 0 and 63 are represented as-is, numbers between
% -64 and -1 have their MSB changed to 0, and anything further from 0 is split
% over multiple bytes.
integer_to_binary_7bit(X) -> append_integer_to_binary(<<>>, X).

append_integer_to_binary(Acc, X) when (-64 =< X) and (X < 64) ->
    <<Acc/binary, (X band 2#01111111)>>;
append_integer_to_binary(Acc, X) ->
    LowByte = 2#10000000 bor (X band 2#01111111),
    NewAcc = <<Acc/binary, LowByte>>,
    append_integer_to_binary(NewAcc, X bsr 7). % arithmetic shift is good

instruction_to_binary({Op, {variable, A}}) ->
    Opcode = opcode(Op),
    ABin = integer_to_binary_7bit(A),
    <<Opcode, ABin/binary>>;
instruction_to_binary({Op, {constant, A}}) ->
    Opcode = opcode(Op) bor 128,
    ABin = integer_to_binary_7bit(A),
    <<Opcode, ABin/binary>>;
instruction_to_binary({Op, {ASort, A}, {BSort, B}}) ->
    Imm1 = case ASort of variable -> 0; constant -> 128 end,
    Imm2 = case BSort of variable -> 0; constant -> 64 end,
    Opcode = opcode(Op) bor Imm1 bor Imm2,
    ABin = integer_to_binary_7bit(A),
    BBin = integer_to_binary_7bit(B),
    <<Opcode, ABin/binary, BBin/binary>>;
instruction_to_binary({Op, {ASort, A}, {BSort, B}, {variable, C}}) ->
    Imm1 = case ASort of variable -> 0; constant -> 128 end,
    Imm2 = case BSort of variable -> 0; constant -> 64 end,
    Opcode = opcode(Op) bor Imm1 bor Imm2,
    ABin = integer_to_binary_7bit(A),
    BBin = integer_to_binary_7bit(B),
    CBin = integer_to_binary_7bit(C),
    <<Opcode, ABin/binary, BBin/binary, CBin/binary>>.

-record(function, {arg_count :: non_neg_integer(),
                   val_count = 0 :: non_neg_integer(),
                   instructions = [] :: [instruction()]}).

create_function(ArgCount) ->
    F = #function{arg_count = ArgCount},
    ArgRefs = [{variable, X} || X <- lists:seq(0, ArgCount - 1)],
    {F, ArgRefs}.

append_instruction(F, Instr) ->
    Intermediates = F#function.val_count,
    Count = F#function.arg_count + Intermediates,
    OutputCount = case Instr of
        {divide_modulo, _, _} -> 2;
        {euclidean_divide_modulo, _, _} -> 2;
        _ -> 1
    end,
    NewF = F#function{instructions = [Instr | F#function.instructions],
                      val_count = Intermediates + OutputCount},
    case OutputCount of
        1 -> {NewF, {variable, Count}};
        2 -> {NewF, {variable, Count}, {variable, Count + 1}}
    end.

% Convenience functions, to write Erlang code that looks similar to the FPE
% code it represents.
add(F, A, B) ->              append_instruction(F, {add, A, B}).
sub(F, A, B) ->              append_instruction(F, {sub, A, B}).
neg(F, A) ->                 append_instruction(F, {neg, A}).
shift_left(F, A, B) ->       append_instruction(F, {shift_left, A, B}).
shift_right(F, A, B) ->      append_instruction(F, {shift_right, A, B}).
mul(F, A, B) ->              append_instruction(F, {mul, A, B}).
divide_modulo(F, A, B) ->    append_instruction(F, {divide_modulo, A, B}).
divide(F, A, B) ->           append_instruction(F, {divide, A, B}).
modulo(F, A, B) ->           append_instruction(F, {modulo, A, B}).
euclidean_divide_modulo(F, A, B) ->
    append_instruction(F, {euclidean_divide_modulo, A, B}).
euclidean_divide(F, A, B) -> append_instruction(F, {euclidean_divide, A, B}).
euclidean_modulo(F, A, B) -> append_instruction(F, {euclidean_modulo, A, B}).
integer_log(F, A) ->         append_instruction(F, {integer_log, A}).
equal(F, A, B) ->            append_instruction(F, {equal, A, B}).
not_equal(F, A, B) ->        append_instruction(F, {not_equal, A, B}).
less(F, A, B) ->             append_instruction(F, {less, A, B}).
greater(F, A, B) ->          append_instruction(F, {greater, A, B}).
less_or_equal(F, A, B) ->    append_instruction(F, {less_or_equal, A, B}).
greater_or_equal(F, A, B) -> append_instruction(F, {greater_or_equal, A, B}).
logical_and(F, A, B) ->      append_instruction(F, {logical_and, A, B}).
logical_or(F, A, B) ->       append_instruction(F, {logical_or, A, B}).
select(F, A, B, C) ->        append_instruction(F, {select, A, B, C}).

append_instruction_list_to_binary(Acc, []) ->
    Acc;
append_instruction_list_to_binary(Acc, [Instr | Rest]) ->
    Next = instruction_to_binary(Instr),
    append_instruction_list_to_binary(<<Acc/binary, Next/binary>>, Rest).

function_to_binary(F) ->
    append_function_to_binary(<<>>, F).

append_function_to_binary(Acc, #function{instructions = Instrs}) ->
    Length = integer_to_binary_7bit(length(Instrs)),
    append_instruction_list_to_binary(<<Acc/binary, Length/binary>>,
                                      lists:reverse(Instrs)).

-record(instance, {port :: port(),
                   state_var_count :: non_neg_integer(),
                   total_var_count :: non_neg_integer()}).

start_instance(Path) ->
    Port = open_port({spawn, Path}, [stream, overlapped_io]),
    #instance{port = Port, state_var_count = 0, total_var_count = 0}.

reset_construction(Instance, Function) ->
    StateVarCount = Function#function.arg_count,
    TotalVarCount = StateVarCount + Function#function.val_count,

    State = [StateVarCount, lists:duplicate(StateVarCount, 0)],

    Construction = append_function_to_binary(<<>>, Function),

    port_command(Instance#instance.port, [1, State, Construction]),

    Instance#instance{state_var_count = StateVarCount,
                      total_var_count = TotalVarCount}.

