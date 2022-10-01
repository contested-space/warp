%%%-------------------------------------------------------------------
%% @doc warp public API
%% @end
%%%-------------------------------------------------------------------

-module(warp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % todo: define an API that makes some sort of sense
    % seriously that /x/y/z is probably literally illegal
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/space_objects/:x/:y/:z", http_space_object_lookup_handler, []},
            {"/space_objects/scan/:x/:y/:z/:r", http_space_object_scan_handler, []},
            {"/characters/:character_id", http_character_lookup_handler, []},
            {"/characters/spawn/:character_id", http_character_spawn_handler, []},
            {"/ship/spawn", http_ship_spawn_handler, []},
            {"/ship/:ship_id/move_to/:x/:y/:z", http_ship_move_handler, []},
            {"/ship/:ship_id/scan", http_ship_scan_handler, []}
        ]}
    ]),
    % todo: configurable port
    {ok, _} = cowboy:start_clear(warp_http_listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    warp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
