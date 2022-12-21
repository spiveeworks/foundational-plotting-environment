-module(port_printloop).
-export([loop/1, start/1]).

start(Path) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, Path}, [stream]),
    loop(Port).

loop(Port) ->
    receive
        {Port, {data, Message}} ->
            io:fwrite("Got message: ~w.~n", [Message]),
            io:fwrite("i.e. \"~s\"~n", [string:chomp(Message)]),
            loop(Port);
        {'EXIT', Port, Reason} ->
            io:fwrite("Port terminated! ~p~n", [Reason]),
            exit(port_terminated)
    end.

