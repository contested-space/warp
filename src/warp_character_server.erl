-module(warp_character_server).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([lookup/1, spawn_character/1]).

-record(state, {characters :: [{binary(), pid()}]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server callbacks
init(_) ->
    InitialState = #state{characters = []},
    {ok, InitialState}.

handle_cast({spawn, CharacterID}, #state{characters = Characters} = State) when is_binary(CharacterID) ->
    CharacterPid = warp_character:spawn(CharacterID),
    {noreply, State#state{characters = [{CharacterID, CharacterPid} | Characters]}};
handle_cast(_, State) ->
    {noreply, State}.

handle_call({lookup, CharacterID}, _From, #state{characters = Characters} = State)
    when is_binary(CharacterID) ->
    case proplists:get_value(CharacterID, Characters, undefined) of
      undefined ->
          % error atom feels weird here, especially when checked at spawning time
          {reply, {error, character_not_found}, State};
      {ok, _CharacterPid} = Res ->
          {reply, Res, State}
    end;
handle_call({lookup, _}, _From, State) ->
    {reply, {error, invalid_character_id}, State};
handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% public functions
-spec lookup(binary()) -> undefined | {ok, pid()}.
lookup(CharacterID) ->
    gen_server:call(?MODULE, {lookup, CharacterID}).

-spec spawn_character(binary()) -> ok.
spawn_character(CharacterID) ->
    gen_server:cast(?MODULE, {spawn, CharacterID}).

