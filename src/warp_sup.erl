%%%-------------------------------------------------------------------
%% @doc warp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(warp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    CharacterServer = #{id => warp_character_server,
               start => {warp_character_server, start_link, []},
               shutdown => 2000,
               restart => permanent,
               type => worker,
               modules => [warp_character_server]},
    ChildSpecs = [CharacterServer],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
