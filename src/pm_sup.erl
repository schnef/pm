%%%-------------------------------------------------------------------
%% @doc pm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pm_sup).

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
    Sup_flags = #{},
    {ok, { Sup_flags, [#{id => pap, start => {pm_pap, start_link, []}},
		       #{id => pdp, start => {pm_pdp, start_link, []}},
		       #{id => epp, start => {pm_epp, start_link, []}},
		       #{id => rap, start => {pm_rap, start_link, []}},
		       #{id => pep_sup,
			 start => {pm_pep_sup, start_link, []},
			 shutdown => infinity,
			 type => supervisor}
		      ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
