-module(warp_ship_server).
-include("warp.hrl").
-include("names.hrl").

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([lookup/1, spawn_ship/0]).

-record(state, {ships :: term()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #state{ships = []}}.

handle_cast({spawn, Id, Position}, #state{ships = Ships} = State) ->
    {ok, Pid} = warp_ship:spawn(Id, Position),
    {noreply, State#state{ships = [{Id, Pid} | Ships]}};
handle_cast(_, State) ->
    {noreply, State}.

handle_call({lookup, ShipId}, _From, #state{ships = Ships} = State) ->
    {ShipId, Pid} = proplists:lookup(ShipId, Ships),
    {reply, {ok, Pid}, State};
handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

spawn_ship() ->
    Id = generate_id(),
    Pos = generate_position(),
    gen_server:cast(?MODULE, {spawn, Id, Pos}),
    Id.

lookup(Id) ->
    gen_server:call(?MODULE, {lookup, Id}).

% TODO: ensure uniqueness
generate_id() ->
    case length(?NUMBERS) of
        N when N > 0 ->
            <<"SHIP_", (lists:nth(rand:uniform(N), ?NUMBERS))/binary, "-",
                (lists:nth(rand:uniform(N), ?NUMBERS))/binary, "-",
                (lists:nth(rand:uniform(N), ?NUMBERS))/binary, "-",
                (lists:nth(rand:uniform(N), ?NUMBERS))/binary>>;
        _ ->
            error(<<"Invalid NUMBERS list">>)
    end.

generate_position() ->
    {50, 50, 50}.
