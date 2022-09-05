-module(http_space_object_scan_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    % EEEEWWW. TODO: anything but that, really. This needs to be properly validated
    Coord = {binary_to_integer(cowboy_req:binding(x, Req0, undefined)),
              binary_to_integer(cowboy_req:binding(y, Req0, undefined)),
              binary_to_integer(cowboy_req:binding(z, Req0, undefined))},
    Radius = binary_to_integer(cowboy_req:binding(r, Req0, undefined)),

    SpaceObjects = scan(Coord, Radius),
    BinaryCoordsSpaceObjects = [<<(integer_to_binary(X))/binary,
                                  "/",
                                  (integer_to_binary(Y))/binary,
                                  "/",
                                  (integer_to_binary(Z))/binary,
                                  "\n">>
                                || {{X, Y, Z}, _Pid} <- SpaceObjects],
    Req = cowboy_req:reply(200, #{}, BinaryCoordsSpaceObjects, Req0),
    {ok, Req, State}.

% private functions
% TODO: obviously, this needs to be a sphere, not a cube
scan(Coord, Radius) ->
    %% Xs = lists:seq(X - Radius, X + Radius),
    %% Ys = lists:seq(Y - Radius, Y + Radius),
    %% Zs = lists:seq(Z - Radius, Z + Radius),

    % Look at me I erlang good herp derp
    %% AllCoords = [{X1, Y1, Z1} || X1 <- Xs, Y1 <- Ys, Z1 <- Zs],
    %% Results = [{Coord, warp_space_object_server:lookup(Coord)} || Coord <- AllCoords],
    %% FilteredResults = [{Coord, Pid} || {Coord, {ok, Pid}} <- Results].
    {ok, Results} = warp_space_object_server:get_sphere(Coord, Radius),
    Results.
    %[warp_space_object:get_state(Pid) || {_, Pid} <- Results].
