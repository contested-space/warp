-module(warp_ship).

-include("warp.hrl").

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([spawn/2, get_state/1, move_to/2, scan/0]).

-record(state, {
    id :: binary(),
    position :: coord(),
    scanner_range = 5 :: non_neg_integer(),
    destination = stopped :: coord() | stopped
}).

init([Id, Position]) ->
    InitialState = #state{id = Id, position = Position},
    {ok, InitialState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

spawn(Id, Position) ->
    gen_server:start(?MODULE, [Id, Position], []).

get_state(ShipPid) ->
    gen_server:call(ShipPid, get_state).

move_to(ShipPid, Coord) ->
    gen_server:call(ShipPid, {move_to, Coord}).

scan() ->
    ok.
