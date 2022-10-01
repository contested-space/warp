-module(warp_space_object).

-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([spawn/1, get_state/1]).

%% TODO: have these names come from ingesting config files
-define(LETTERS,
        [ << "Alpha" >> , << "Beta" >> , << "Gamma" >> , << "Delta" >> , << "Epsilon" >> , << "Varepsilon" >> , << "Zeta" >> , << "Eta" >> , << "Theta" >> , << "Iota" >> , << "Kappa" >> , << "Lambda" >> , << "Mu" >> , << "Nu" >> , << "Xi" >> , << "Omicron" >> , << "Pi" >> , << "Rho" >> , << "Sigma" >> , << "Tau" >> , << "Upsilon" >> , << "Phi" >> , << "Varphi" >> , << "Chi" >> , << "Psi" >> , << "Omega" >> ]).
-define(NAMES,
        [ << "Andromeda" >> , << "Antila" >> , << "Apus" >> , << "Aquarius" >> , << "Aquila" >> , << "Ara" >> , << "Aries" >> , << "Auriga" >> , << "Bootes" >> , << "Caelum" >> , << "Camelopardalis" >> , << "Canes Venatici" >> , << "Canis Major" >> , << "Canis Minor" >> , << "Capricornus" >> , << "Carina" >> , << "Cassiopeia" >> , << "Centauri" >> ]).

-record(state,
        {name :: binary(),
         type :: planet | asteroid | starbase,
         affiliation :: none | binary(),
         position :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
         resources :: []}).

%% Yeah I can't keep doing that kind of argument nonsense, I need to clean my stuff up ASAP
init([Position]) ->
    {Letter, Name} = case {length(?LETTERS), length(?NAMES)} of
                       {N, M} when N > 0 andalso M > 0 ->
                           {lists:nth(rand:uniform(N), ?LETTERS), lists:nth(rand:uniform(N), ?NAMES)};
                       _ ->
                           error(<<"Invalid LETTERS or NAMES list">>)
                     end,
    InitialState = #state{name = <<Letter/binary, " ", Name/binary>>,
                          type = planet,
                          affiliation = none,
                          position = Position,
                          resources = []},
    {ok, InitialState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(get_state,
            _From,
            #state{name = Name, type = Type, affiliation = Affiliation, resources = Res} = State) ->
    StateMap = #{name => Name, type => Type, affiliation => Affiliation, resources => Res},
    {reply, {ok, StateMap}, State};
handle_call(_, _From, State) ->
    {reply, {error, undefined_call}, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

spawn(Position) ->
    gen_server:start(?MODULE, [Position], []).

get_state(SpaceObjectPid) ->
    gen_server:call(SpaceObjectPid, get_state).

