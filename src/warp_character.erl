-module(warp_character).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([spawn/1, get_state/1]).

%% todo: use a regular record as private state, and provide
%% subset of that as a map or proplist via get_state (or get_status)

%% todo: this needs to be supervised by something!!!

%% -record(state, {id :: binary(),
%%                 time_of_birth :: non_neg_integer(),
%%                 current_task :: atom(),
%%                 leader :: none | binary(),
%%                 followers :: [binary()],
%%                 resources :: []}).

% todo: manage arguments a bit better than that

% todo: have a character "constructor" that generate
% the character ID based on things (origin? time?)

% todo: have a character "constructor" that uses pre-established
% parameters (to allow for storage of characters and respawning of their
% processes)
init([CharacterId]) ->
    InitialState = #{id => CharacterId,
                     time_of_birth => erlang:system_time(),
                     current_task => idling,
                     leader => none,
                     followers => [],
                     resources => []},
    {ok, InitialState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

spawn(CharacterId) ->
    gen_server:start(?MODULE, [CharacterId], []).

get_state(CharacterPid) ->
    gen_server:call(CharacterPid, get_state).

