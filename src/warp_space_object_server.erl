-module(warp_space_object_server).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([lookup/1, get_sphere/2]).

-record(state, {space_objects :: term()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server callbacks
init(_) ->
    EmptyWorld = kd_tree:new(),
    InitialState = #state{space_objects = EmptyWorld},
    self() ! populate,
    {ok, InitialState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call({lookup, {_, _, _} = Pos}, _From, #state{space_objects = SpaceObjects} = State) ->
    case kd_tree:lookup(Pos, SpaceObjects) of
      {error, key_not_found} ->
          % error atom feels weird here, especially when checked at spawning time
          {reply, {error, space_object_not_found}, State};
      {ok, SpaceObjectPid} ->
          {reply, {ok, SpaceObjectPid}, State}
    end;
handle_call({get_sphere, Coord, Radius}, _From, #state{space_objects = SpaceObjects} = State) ->
    Results = kd_tree:get_sphere(Coord, Radius, SpaceObjects),
    {reply, {ok, Results}, State};
handle_call({lookup, _}, _From, State) ->
    {reply, {error, invalid_character_id}, State};
handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(populate, State) ->
    SpaceObjects = populate(10000),
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

get_sphere(Coord, Radius) ->
    gen_server:call(?MODULE, {get_sphere, Coord, Radius}).

% private functions
populate(N) ->
    populate(N, kd_tree:new()).

% TODO: have parameters for the size of the universe
% TODO: avoid duplicates
populate(0, Tree) ->
    Tree;
populate(N, Tree) ->
    try Coord = {rand:uniform(1000), rand:uniform(1000), rand:uniform(1000)},
        {ok, Pid} = warp_space_object:spawn(Coord),
        Info = warp_space_object:get_state(Pid),
        io:format("Space object created: ~p~n", [Info]),
        NewTree = kd_tree:insert(Coord, Pid, Tree)
    of
      _ ->
          populate(N - 1, NewTree)
    catch
      _:_ -> % just retry when there is already something there
          populate(N, Tree)
    end.

