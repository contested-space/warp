-module(warp_space_object_server).

-behaviour(gen_server).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/0]).

-export([lookup/1]).

-record(state, {space_objects :: []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server callbacks
init(_) ->
    InitialState = #state{space_objects = []},
    self() ! populate,
    {ok, InitialState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call({lookup, {_, _, _} = Pos}, _From, #state{space_objects = SpaceObjects} = State) ->
    case proplists:get_value(Pos, SpaceObjects, undefined) of
        undefined ->
            % error atom feels weird here, especially when checked at spawning time
            {reply, {error, space_object_not_found}, State};
        SpaceObjectPid ->
            {reply, {ok, SpaceObjectPid}, State}
    end;
handle_call({lookup, _}, _From, State) ->
    {reply, {error, invalid_character_id}, State};
handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(populate, State) ->
    SpaceObjects = populate(1000),
    {noreply, State#state{space_objects = SpaceObjects}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% public functions
lookup(SpaceObjectPosition) ->
    gen_server:call(?MODULE, {lookup, SpaceObjectPosition}).

% private functions
populate(N) ->
    populate(N, []).

% TODO: have parameters for the size of the universe
% TODO: avoid duplicates
populate(0, List) -> List;
populate(N, List) ->
    Coord = {rand:uniform(100), rand:uniform(100), rand:uniform(100)},
    {ok, Pid} = warp_space_object:spawn(Coord),
    Info = warp_space_object:get_state(Pid),
    io:format("Space object created: ~p~n", [Info]),
    populate(N - 1, [{Coord, Pid} | List]).
