
-module(print).

-export([f/1]).

f({json, Json}) ->
    Bits = list_to_binary(Json),
    io:fwrite("~ts~n", [Bits]);
f(Error) ->
    io:fwrite("~p~n", [Error]).
