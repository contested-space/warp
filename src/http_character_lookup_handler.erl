-module(http_character_lookup_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
     Req = case cowboy_req:binding(character_id, Req0, undefined) of
               undefined ->
                   cowboy_req:reply(400, #{}, <<>>, Req0);
               ID ->
                   case warp_character_server:lookup(Id) of
                       {error, _} ->
                           cowboy_req:reply(204, #{}, <<>>, Req0);
                       {ok, CharacterPid} ->
                           {ok, CharacterState} = warp_character:get_state(CharacterPid),
                           StateJson = jiffy:encode(CharacterState),
                           cowboy_req:reply(200, #{}, <<StateJson/binary, "\n">>, Req0)
                   end
           end,
    {ok, Req, State}.
