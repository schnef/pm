%%%-------------------------------------------------------------------
%% @doc pm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pm_pccache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Sup_flags = #{strategy => simple_one_for_one,
		  intensity => 0,
		  period => 1},
    Child_specs = [#{id => cachel,
		     start => {pm_pccache, start_link, []},
		     shutdown => brutal_kill}],
    {ok, {Sup_flags, Child_specs}}.

%%====================================================================
%% Internal functions
%%====================================================================
