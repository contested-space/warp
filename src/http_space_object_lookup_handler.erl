-module(http_space_object_lookup_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    % EEEEWWW. TODO: anything but that, really. This needs to be properly validated
    Coords = {binary_to_integer(cowboy_req:binding(x, Req0, undefined)),
              binary_to_integer(cowboy_req:binding(y, Req0, undefined)),
              binary_to_integer(cowboy_req:binding(z, Req0, undefined))},
    Req = case warp_space_object_server:lookup(Coords) of
            {error, _} ->
                cowboy_req:reply(204, #{}, <<>>, Req0);
            {ok, SpaceObjectPid} ->
                {ok, SpaceObjectState} = warp_space_object:get_state(SpaceObjectPid),
                StateJson = utils:ensure_binary(jiffy:encode(SpaceObjectState)),
                cowboy_req:reply(200, #{}, <<StateJson/binary, "\n">>, Req0)
          end,
    {ok, Req, State}.

