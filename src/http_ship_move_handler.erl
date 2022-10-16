-module(http_ship_move_handler).
-include("warp.hrl").
-include("ship.hrl").

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    % EEEEWWW. TODO: anything but that, really. This needs to be properly validated
    MoveCoord = {
        binary_to_integer(cowboy_req:binding(x, Req0, undefined)),
        binary_to_integer(cowboy_req:binding(y, Req0, undefined)),
        binary_to_integer(cowboy_req:binding(z, Req0, undefined))
    },
    ShipId = cowboy_req:binding(ship_id, Req0, undefined),

    {ok, ShipPid} = warp_ship_server:lookup(ShipId),
    #ship_state{position = StartCoord} = warp_ship:get_state(ShipPid),
    {ok, Distance} = warp_ship:move_to(ShipPid, MoveCoord),
    Message = io_lib:format("You moved ~p space units from ~p to ~p\n", [
        Distance, StartCoord, MoveCoord
    ]),
    MessageBin = list_to_binary(lists:flatten(Message)),
    Req = cowboy_req:reply(200, #{}, <<MessageBin/binary>>, Req0),
    {ok, Req, State}.
