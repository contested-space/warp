-module(http_character_spawn_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req =
        case cowboy_req:binding(character_id, Req0, undefined) of
            undefined ->
                cowboy_req:reply(400, #{}, <<>>, Req0);
            ID ->
                case warp_character_server:lookup(ID) of
                    {error, character_not_found} ->
                        warp_character_server:spawn_character(ID),
                        cowboy_req:reply(200, #{}, <<>>, Req0);
                    {ok, _CharacterPid} ->
                        cowboy_req:reply(304, #{}, <<>>, Req0)
                end
        end,
    {ok, Req, State}.
