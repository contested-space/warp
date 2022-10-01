-module(utils).

-export([ensure_binary/1]).

-spec ensure_binary(term()) -> binary().
ensure_binary(Val) when is_binary(Val) ->
    Val;
ensure_binary(Val) ->
    error(io_lib:format("Expected binary, got: ~p", [Val])).
