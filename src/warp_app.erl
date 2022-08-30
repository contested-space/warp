%%%-------------------------------------------------------------------
%% @doc warp public API
%% @end
%%%-------------------------------------------------------------------

-module(warp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % todo: define an API that makes some sort of sense
    Dispatch = cowboy_router:compile([{'_', [
        {"/characters/:character_id", http_character_lookup_handler, []},
        {"/characters/spawn/:character_id", http_character_spawn_handler, []}]}]),

    % todo: configurable port
    {ok, _} = cowboy:start_clear(warp_http_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}
                                ),

    warp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
