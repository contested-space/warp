-module(warp_ship).

-include("warp.hrl").
-include("ship.hrl").

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([spawn/2, get_state/1, move_to/2, scan/0]).

init([Id, Position]) ->
    InitialState = #ship_state{id = Id, position = Position},
    {ok, InitialState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({move_to, {X0, Y0, Z0} = Coord}, _From, #ship_state{position = {X1, Y1, Z1}} = State) ->
    Distance = math:sqrt(math:pow((X1 - X0), 2) + math:pow((Y1 - Y0), 2) + math:pow((Z1 - Z0), 2)),
    {reply, {ok, Distance}, State#ship_state{position = Coord}};
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
