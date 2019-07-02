-module(pm_pep_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add/1, delete/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

add(User) ->
    Child = #{id => User, start => {pm_pep, start_link, [User]}},
    supervisor:start_child(?SERVER, Child).
    
delete(Pid) ->
    supervisor:terminate_child(?SERVER, Pid),
    supervisor:delete_child(?SERVER, Pid).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, { #{strategy => one_for_one}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
