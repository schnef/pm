-module(pm_pccache).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include("pm.hrl").

-export([start_link/1, insert_node/1, insert_edge/2, del_node/1, del_edge/2,
	 find_pc_set/1, clear/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(Time, 1000). % Time period for periodically PCcache updates

-record(state, {g :: digraph:graph(), cache, time :: pos_integer(), erased_nodes :: [pm:id()]}).

%% @doc This updates the PCcache to make sure it is consistent in the
%% event that a node is removed from the access control graph. It
%% doesn't matter what type of node is deleted.
del_node(X) ->
    gem_server:cast(?SERVER, {del_node, X}).

%% @doc This updates the PCcache to make sure it is consistent in the
%% event that an edge is removed from the access control graph. If
%% [x,y] is not a ua->oa edge, then update_PCcache_del_node(x).
del_edge(X, Y) ->
    gem_server:cast(?SERVER, {del_edge, X, Y}).

%% @doc This updates the PCcache to make sure it is consistent in the
%% event that a node is added to the access control graph
insert_node(X) ->
    gen_server:cast(?SERVER, {insert_node, X}).

%% @doc This updates the PCcache to make sure it is consistent in the
%% event that an edge is added to the access control graph
insert_edge(X, Y) ->
    gen_server:cast(?SERVER, {insert_edge, X, Y}).
    
%% @doc This returns the set of PC nodes that are reachable from X
find_pc_set(X) ->
    gen_server:call(?SERVER, {find_pc_set, X}).
    
%% @doc Clear the cache
%% TODO: What about restarting the cache? This will destroy whatever
%% the cache holds = good and also will terminate all processes which
%% are waiting on it.
clear(G) ->
    gen_server:cast(?SERVER, {clear, G}).
    
%% @doc Start server
start_link(G) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [G], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(G) ->
    Cache = new(),
    erlang:start_timer(?Time, self(), cache_update),
    {ok, #state{g = G, cache = Cache, erased_nodes = [], time = ?Time}}.

%% @private
handle_call({find_pc_set, X}, _From, #state{g = G, cache = Cache} = State) ->
    PCs = find_pc_set(G, Cache, X),
    {reply, {ok, PCs}, State};
handle_call(Request, _From, State) ->
    Reply = {error, {not_implemented, Request}},
    {reply, Reply, State}.

%% @private
handle_cast({del_node, X},
	    #state{g = G, cache = Cache, erased_nodes = Erased_nodes} = State) ->
    Erased_nodes1 = Erased_nodes ++ del_node(G, Cache, X),
    {noreply, State#state{erased_nodes = Erased_nodes1}};
%% Because in this implementation X, Y relations of the kind ua->oa
%% are not kept in the digraph, but as associations in the database,
%% this function is equivalent to del_node(X). Calling del_node(X)
%% *will* delete Y if Y is reachable from X.
handle_cast({del_edge, X, _Y},
	    #state{g = G, cache = Cache, erased_nodes = Erased_nodes} = State) ->
    Erased_nodes1 = Erased_nodes ++ del_node(G, Cache, X),
    {noreply, State#state{erased_nodes = Erased_nodes1}};
handle_cast({insert_node, X},
	    #state{g = G, cache = Cache} = State) ->
    insert_edge(G, Cache, X, X),
    {noreply, State};
handle_cast({insert_edge, X, Y},
	    #state{g = G, cache = Cache} = State) ->
    insert_edge(G, Cache, X, Y),
    {noreply, State};
handle_cast({clear, G2}, #state{cache = Cache1} = State) ->
    ets:delete(Cache1),
    Cache2 = new(),
    {noreply, State#state{g = G2, cache = Cache2}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, cache_update},
	    #state{g = G, cache = Cache, time = Time, erased_nodes = Erased_nodes} = State) ->
    Nodes = [{X, lists:sort(find_pc_set(G, X))}
	     || X <- Erased_nodes,
		not ets:member(Cache, X)], % in case already added in the meantime
    case Nodes of
	[] -> ok;
	_ -> ets:insert(Cache, Nodes)
    end,
    erlang:start_timer(Time, self(), cache_update),
    {noreply, State#state{erased_nodes = []}}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Create a new cache.
new() ->
    ets:new(cache, [set, named_table, protected]).

%% find_pc_set:
%% 1. If startnode is in PCcache, return PCcache[startnode] (i.e., the
%%    set of PC nodes reachable from startnode)
%% 2. If startnode is not in PCcache
%%     1. BFS from startnode to find set of applicable PC nodes (do not
%%        traverse any ua->oa edges)
%%     2. Let PCcache[startnode] equal the set of discovered PC nodes
%%     3. Return set of discovered PC nodes
find_pc_set(G, Cache, X) ->
    case ets:lookup(Cache, X) of
	[{_X, PCs}] ->
	    PCs;
	[] ->
	    PCs = lists:sort(find_pc_set(G, X)),
	    ets:insert(Cache, {X, PCs}),
	    PCs
    end.

find_pc_set(G, AT) ->
    [PC || {pc, _} = PC <- digraph_utils:reachable_neighbours([AT], G)].

%% @doc insert_edge
%% 1. PCset=find_pc_set(y)
%% 2. if x in PCcache then PCcache[x] = PCcache[x] union PCset
%% 3. BFS backwards from x (without traversing any ua->oa edges) to find
%%    set of all reachable nodes
%% 4. For each reachable node z, if z in PCcache then PCcache[z] =
%%    PCcache[z] union PCset
%%
%% F implements the functionality: if x in PCcache then PCcache[x] =
%% PCcache[x] union PCset, which now reduces lines 2 and 4 to a call
%% to F for all reachable nodes, including X itself, which is what the
%% call digraph_utils:reachable does.
%%
%% TODO: only cached nodes are updated and non-cached nodes are
%% ignored, i.e. non-cached nodes are not added to the cache as a side
%% effect. Good or bad?
insert_edge(G, Cache, X, Y) ->
    PCs_y = find_pc_set(G, Cache, Y),
    F = fun(Node, Acc) ->
		case ets:lookup(Cache, Node) of
		    [{_Node, PCs_node}] ->
			PCs_node1 = lists:merge(PCs_y, PCs_node),
			[{Node, PCs_node1} | Acc];
		    [] ->
			Acc
		end
	end,
    L = [Z || {Tag, _} = Z <- digraph_utils:reachable([X], G), Tag =/= pc],
    ets:insert(Cache, lists:foldl(F, [], L)).

%% @doc del_node
%% 1. erase PCcache[thenode]
%% 2. BFS backwards (without traversing any ua to oa edges) to find set
%%    of all reachable nodes
%% 3. For each reachable node x, erase PCcache[x]
%% 4. Add each erased node to an 'erased node' list
%%
%% TODO: should the erased modes list be a queue because a list is a
%% filo stack which makes the first nodes erased to be the last ones
%% added to the cache.
del_node(G, Cache, X) ->
    case ets:member(X, Cache) of
	true ->
	    [begin
		 ets:delete(X, Cache),
		 Y
	     end
	     || {Tag, _} = Y <- digraph_utils:reachable([X], G), % Includes X
		Tag =/= pc];
	false ->
	    []
    end.

-ifdef(EUNIT).

server_test_() ->
    {setup,
     fun server_setup/0,
     fun server_cleanup/1,
     fun tsts/1}.

server_setup() ->
    zuuid:start(),
    {ok, Cwd} = file:get_cwd(), % current working directory
    Uuid = zuuid:string(zuuid:v1()), % generate a UUID
    Dir = filename:join([Cwd, "test", Uuid]), % construct temporary directory
    ok = file:make_dir(Dir),
    %% Tell Mnesia where to put the temporary database and also
    %% increase the dump_log_write_threshold to suppress overload
    %% warnings.
    application:set_env(mnesia, dir, Dir),
    application:set_env(mnesia, dump_log_write_threshold, 5000),
    pm_db:install([node()]), % install a fresh db
    application:ensure_all_started(pm),
    Dir.

server_cleanup(Dir) ->
    application:stop(pm),
    application:stop(mnesia),
    {ok, Filenames} = file:list_dir(Dir),
    [file:delete(filename:join([Dir, Filename])) || Filename <- Filenames],
    file:del_dir(Dir),
    ok.

tsts(_Pids) ->
    [
    ].


-endif.
