-module(http_ship_spawn_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    Id = warp_ship_server:spawn_ship(),
    cowboy_req:reply(200, #{}, <<Id/binary>>, Req),
    {ok, Req, State}.
