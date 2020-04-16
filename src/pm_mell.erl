-module(pm_mell).

-include("pm.hrl").

-compile(export_all).

-export([gv/1]).
-export([find_border_oa_priv_ANSI/2, find_border_oa_priv_RESTRICTED/2,
	 calc_priv/3, access/4, show_accessible_objects/2, vis_initial_oa_ANSI/2,
	 vis_initial_oa_RESTRICTED/2, predecessor_oa/3, successor_oa/3,
	 find_orphan_objects/2, show_ua/2, check_prohibitions/5]).

%% This function takes a digraph and outputs dot formatted output for displaying the graph.
gv(G) ->
    PEs = digraph_utils:topsort(G),
    F1 = fun({u, _} = Id, {Acc, Iu, Iua, Io, Ioa, Ipc}) ->
		 U = "u" ++ integer_to_list(Iu),
		 [#u{value = L}] = mnesia:dirty_read(u, Id),
		 {[{Id, U, L} | Acc], Iu + 1, Iua, Io, Ioa, Ipc};
	    ({ua, _} = Id, {Acc, Iu, Iua, Io, Ioa, Ipc}) ->
		 UA = "ua" ++ integer_to_list(Iua),
		 [#ua{value = L}] = mnesia:dirty_read(ua, Id),
		 {[{Id, UA, L} | Acc], Iu, Iua + 1, Io, Ioa, Ipc};
	    ({o, _} = Id, {Acc, Iu, Iua, Io, Ioa, Ipc}) ->
		 O = "o" ++ integer_to_list(Io),
		 [#o{value = L}] = mnesia:dirty_read(o, Id),
		 {[{Id, O, L} | Acc], Iu, Iua, Io + 1, Ioa, Ipc};
	    ({oa, _} = Id, {Acc, Iu, Iua, Io, Ioa, Ipc}) ->
		 OA = "oa" ++ integer_to_list(Ioa),
		 [#oa{value = L}] = mnesia:dirty_read(oa, Id),
		 {[{Id, OA, L} | Acc], Iu, Iua, Io, Ioa + 1, Ipc};
	    ({pc, _} = Id, {Acc, Iu, Iua, Io, Ioa, Ipc}) ->
		 PC = "pc" ++ integer_to_list(Ipc),
		 [#pc{value = L}] = mnesia:dirty_read(pc, Id),
		 {[{Id, PC, L} | Acc], Iu, Iua, Io, Ioa, Ipc + 1}
	 end,
    {Acc, _, _, _, _, _} = lists:foldl(F1, {[], 1, 1, 1, 1, 1}, PEs),
    M = maps:from_list([{Id, Name} || {Id, Name, _Label} <- Acc]),
    F2 = fun({_Id, Name, undefined}) ->
		 io_lib:format("  ~s [label=\"<~s>\" shape=plaintext]", [Name, Name]);
	    ({_Id, Name, Label}) ->
		 io_lib:format("  ~s [label=\"~s\" shape=plaintext]", [Name, Label])
	 end,
    Ns = lists:map(F2, Acc),

    PCs = [io_lib:format("  ~s -> ~s", [maps:get(Id2, M), Name])
	   || {{pc, _} = Id1, Name, _Label} <- Acc,
	      Id2 <- digraph:in_neighbours(G, Id1)],

    Us = [io_lib:format("  ~s -> ~s", [maps:get(Id2, M), Name])
	  || {{ua, _} = Id1, Name, _Label} <- Acc,
	     {Tag, _} = Id2 <- digraph:in_neighbours(G, Id1),
	     Tag =:= u orelse Tag =:= ua],

    Os = [io_lib:format("  ~s -> ~s", [maps:get(Id2, M), Name])
	  || {{oa, _} = Id1, Name, _Label} <- Acc,
	     {Tag, _} = Id2 <- digraph:in_neighbours(G, Id1),
	     Tag =:= o orelse Tag =:= oa],

    %% Os = [io_lib:format("  ~s -> ~s", [maps:get(Id2, M), Name])
    %% 	  || {Id1, Name, _Label} <- Acc,
    %% 	     {Tag, _} = Id2 <- digraph:in_neighbours(G, Id1),
    %% 	     Tag =:= o orelse Tag =:= oa],

    F3 = fun({{ua, _} = Id, _Name, _Label}, Acc) ->
		 mnesia:dirty_read(association, Id) ++ Acc;
	    (_, Acc) ->
		 Acc
    	 end,
    Assocs = lists:foldl(F3, [], Acc),
    As = [io_lib:format("  ~s -> ~s [constraint=false, splines=curved, style=dashed]",
			[maps:get(UA, M), maps:get(AT, M)])
	  || #association{ua = UA, at = AT} <- Assocs],
    {ok, S} = file:open("pm.gv", write),
    io:format(S,
	      "digraph pm {~n"
	      "  subgraph cluster0 {~n"
	      "  color=blue;~n"
	      "  label=\"Objects\";~n"
	      "~s;~n}~n~n"
	      "  subgraph cluster1 {~n"
	      "  color=green;~n"
	      "  label=\"Users\";~n"
	      "~s;~n}~n~n"
	      "/* connect to PCs */~n"
	      "  subgraph cluster2 {~n"
	      "  label=\"Policy classes\";~n"
	      "~s;~n}~n~n"
	      "/* Associations */~n"
	      "~s;~n~n"
	      "/* Node attributes */~n"
	      "~s;~n"
	      "~n}~n", [lists:join(";\n", Os),
			lists:join(";\n", Us),
			lists:join(";\n", PCs),
			lists:join(";\n", As),
			lists:join(";\n", Ns)]),
    file:close(S).

%% =============================================================================
%% The following functions are taken from the document "English Prose
%% Access Control Graph Algorithms with Complexity Analysis" by Peter
%% Mell.
%% =============================================================================

-spec find_border_oa_priv_ANSI(G, U) -> ATnodes when
      G :: digraph:graph(),
      U :: pm:id() | pm:u(),
      ATnodes :: [{AT_id, [{PC_id, [AR_id]}]}],
      AT_id :: pm:id(),
      AR_id :: pm:id(),
      PC_id :: pm:id().
%% @doc This function returns all oa _and_ ua nodes that are
%% successors of ua nodes through association (for ua nodes reachable
%% from u). THIS FOLLOWS THE ANSI PRIVILEGE PM SPECIFICATION. It
%% labels each returned node with the reachable PC nodes paired with
%% the access rights conferred by the ua->oa edges.
find_border_oa_priv_ANSI(G, U) ->
    find_border_oa_priv(G, U, false).

-spec find_border_oa_priv_RESTRICTED(G, U) -> ATnodes when
      G :: digraph:graph(),
      U :: pm:id() | pm:u(),
      ATnodes :: [{AT_id, [{PC_id, [AR_id]}]}],
      AT_id :: pm:id(),
      AR_id :: pm:id(),
      PC_id :: pm:id().
%% @doc This returns all oa _and_ ua nodes that are successors by
%% association of ua nodes (for ua nodes reachable from u). THIS
%% FOLLOWS THE NIST RESTRICTED VERSION OF THE ANSI PM PRIVILEGE
%% SPECIFICATION. It labels each returned node with the access right,
%% PC node pairings of the predecessor ua nodes filtered to include
%% only pairs where the PC nodes are reachable from the respective oa
%% node.
find_border_oa_priv_RESTRICTED(G, U) ->
    find_border_oa_priv(G, U, true).

find_border_oa_priv(G, #u{id = U}, Restricted) ->
    find_border_oa_priv(G, U, Restricted);
find_border_oa_priv(G, U, Restricted) ->
    %% Search from u to find set of ua nodes. Whether the
    %% reachable_neighbours/2 uses a BFS (breath first search) is
    %% unknown. It does however return all elements in the graph that
    %% have a path of length one or more from U. U is not included in
    %% the returned list.
    UAs = [UA || {ua, _} = UA <- digraph_utils:reachable_neighbours([U], G)],

    %% Identify the ‘active’ ua to at bridge edges.
    F1 = fun(UA, Acc) ->
		 mnesia:dirty_read(association, UA) ++ Acc
    	 end,
    Active = lists:foldl(F1, [], UAs),
    
    %% For restricted mode, for each discovered ua node, x, execute
    %% find_pc_set(x) to find applicable set of PC nodes
    Active1 = case Restricted of
		  true ->
		      [{Assoc, find_pc_set(G, UA)} || #association{ua = UA} = Assoc <- Active];
		  false ->
		      [{Assoc, undefined} || Assoc <- Active]
	      end,
    
    %% For each edge, label the oa node with the access rights
    %% conferred by the edge. Traverse the 'active' ua -> at edges and
    %% add the ARset of each association to the at, merging duplicate
    %% ARsets. At this point, we only use the ids of the ARsets and
    %% not the content of the sets yet. The result is the list of at's
    %% the ua's are pointing to with for each at a list of ARsets.
    F2 = fun({#association{at = AT, arset = ARset}, PCs_appl}, Acc) ->
    		 case lists:keytake(AT, 1, Acc) of
    		     {value, {_AT, ARsets}, Acc1} ->
    			 [{AT, lists:merge(ARsets, [ARset]), PCs_appl} | Acc1];
    		     false ->
    			 [{AT, [ARset], PCs_appl} | Acc]
    		 end
    	 end,
    Active2 = lists:foldl(F2, [], Active1),
    
    %% For each reached at node, x, execute find_pc_set(x) to find set
    %% of reachable PC nodes.
    %%
    %% For the unrestricted mode, pair each access right with each reached
    %% PC node to create access right, PC node pairings. The result is
    %% a list of tuples, each tuple referring to a AT in the first
    %% element and a list of {access right, PC node pairings} as the
    %% second element.
    %%
    %% For the restricted mode, do the above but restrict the PCs to
    %% those the UA and AT share.
    %%
    %% The result is a list with tuples, with each tuple refering to
    %% an AT as the first element and the second element a list of
    %% tuples {ARset, PC}.
    Active3 = [{AT, [{ARset, PC} || ARset <- ARsets,
				    PC <- find_pc_set(G, AT),
				    not Restricted orelse lists:member(PC, PCs_appl)]}
	       || {AT, ARsets, PCs_appl} <- Active2],

    %% Now merge the ARsets per PC per AT. The function merges ARsets per PC.
    F3 = fun({ARset, PC}, Acc) ->
		 case Acc of
		     #{PC := Set1} ->
			 Set1;
		     _ ->
			 Set1 = sets:new()
		 end,
		 [#set{value = Set2}] = mnesia:dirty_read(arset, ARset),
    		 Set3 = sets:union(Set1, Set2),
		 Acc#{PC => Set3}
    	 end,
    [{AT, [{PC, ARs} || {PC, ARs} <- maps:to_list(lists:foldl(F3, #{}, Pairs))]}
     || {AT, Pairs} <- Active3].
    
find_pc_set(G, AT) ->
    [PC || {pc, _} = PC <- digraph_utils:reachable_neighbours([AT], G)].

-spec calc_priv(G, U ,OA) -> [AR_id] when
      G :: digraph:graph(),
      U :: pm:id() | pm:u(),
      OA :: pm:id(),
      AR_id :: pm:id().
%% @doc This function returns all privileges that u has on oa
calc_priv(G, U ,OA) ->
    %% From oa, BFS to find all reachable oa border and other oa
    %% nodes. Pick up the PCs at the same time and put them in two
    %% different lists.
    %%
    %% TODO: Is it correct to assume that the OA itself may be a
    %% border OA? I.e. use `digraph_utils:reachable' instead of
    %% `digraph_utils:reachable__neighbours'.
    F1 = fun({o, _} = X, {Xs, Ys}) ->
		 {[X | Xs], Ys};
	    ({oa, _} = X, {Xs, Ys}) ->
		 {[X | Xs], Ys};
	    ({pc, _} = Y, {Xs, Ys}) ->
		 {Xs, [Y | Ys]}
	 end,
    {OAs, PCs} = lists:foldl(F1, {[], []}, digraph_utils:reachable([OA], G)),
    
    io:format("++ pcs ~p~n", [PCs]),
    io:format("++ oas ~p~n", [OAs]),
    %% execute find_border_oa_priv(u) (either the ANSI or NIST
    %% RESTRICTED version) to find the set of 'oa border nodes' Only
    %% take into account the OA border nodes which we found in the
    %% previous step.
    AT_nodes1 = find_border_oa_priv_RESTRICTED(G, U),
    io:format("++ at_nodes ~p~n", [AT_nodes1]),
    ARsets = [ARset || {AT, Pairs} = AT_node <- AT_nodes1,
		       lists:member(AT, OAs),
		       {PC, ARset} <- Pairs,
		       PC =:= undefined orelse lists:member(PC, PCs)],
    F2 = fun(S1, S2) ->
		 sets:union(S1, S2)
	 end,
    sets:to_list(lists:foldl(F2, sets:new(), ARsets)).

%% @doc This function determines whether or not u has privilege op on o.
access(G, U, Op, O) ->
    [].

%% @doc This function finds the set of objects accessible to u. This
%% would be used, for example, if the user wanted to do a keyword
%% search on all accessible object.
show_accessible_objects(G, U) ->
    [].

%% @doc Returns the initial set of oa nodes to display when a user
%% wants to explore their files (and use the access graph structure as
%% a default way to explore them). THIS FOLLOWS THE ANSI PRIVILEGE PM
%% SPECIFICATION.
vis_initial_oa_ANSI(G, U) ->
    [].

%% @doc Returns the initial set of oa nodes to display when a user
%% wants to explore their files (and use the access graph structure as
%% a default way to explore them). THIS FOLLOWS THE NIST RESTRICTED
%% VERSION OF THE ANSI PM PRIVILEGE SPECIFICATION.
vis_initial_oa_RESTRICTED(G, U) ->
    [].

%% @doc This returns the next hierarchical level of oa nodes to
%% display given a user and a target object attribute (using the
%% access graph structure as a default way to explore the file
%% structure). Note: The oa input parameter is the entry that user, u,
%% clicked. We assume that u has the privilege to see oa in the
%% directory tree if this method is invoked.
predecessor_oa(G, U ,OA) ->
    [].

%% @doc This returns the set of valid parent nodes for oa given that
%% the user is u.
successor_oa(G, U, OA) ->
    [].

%% @doc This returns the set of nodes that are accessible by u but not
%% reachable through the visualization interface because one or more
%% of the intervening object attribute nodes are not accessible. This
%% is not expected to be a normal occurrence (and does not occur in
%% any of our current example graphs).
find_orphan_objects(G, U) ->
    [].

%% @doc This returns the set of descendants for node ua. Note, ua may
%% actual be a user instead of a user attribute.
show_ua(G, UA) ->
    [].





%% TDOO: improve specs by giving a more specific type for Candidates,
%% ATIs etc.
-spec check_prohibitions(G, Candidates, Type, ATIs, ATEs) -> [pc:id()] when
      G :: digraph:graph(),
      Candidates :: sets:set() | [pm:id()],
      Type :: disjunctive | conjunctive,
      ATIs :: [pm:id()],
      ATEs :: [pm:id()].
%% @doc The function `check_prohibitions/5' was taken, literally, from
%% a document titled "English Prose Access Control Graph Algorithms
%% with Complexity Analysis" by Peter Mell, dated 2016-04-12.
check_prohibitions(G, Candidates, Type, ATIs, ATEs) when is_list(Candidates) ->
    check_prohibitions(G, sets:from_list(Candidates), Type, ATIs, ATEs);
check_prohibitions(G, Candidates, disjunctive, ATIs, ATEs) when is_list(ATIs), is_list(ATEs) ->
    %% Step a: for each ati in atis, compute the union of element(ati).
    S1 = elements_union(G, ATIs),
    %% If any nodes in candidates are in the computed union, delete
    %% them from candidates, i.e. subtract the collected elements from
    %% the candidates.
    Candidates1 = sets:subtract(Candidates, S1),
    %% Step b: for each ate in ates, compute the intersection of
    %% elements(ate).
    S2 = elements_intersection(G, ATEs),
    %% If any nodes in candidates are not in the computed
    %% intersection, delete them from candidates. This effectively is
    %% the same as taking the intersection of the collected set and
    %% the candidates.
    Candidates2 = sets:intersection(Candidates1, S2),
    %% Step c: return remaining nodes in candidates
    sets:to_list(Candidates2);
check_prohibitions(G, Candidates, conjunctive, ATIs, ATEs) ->
    %% Step a: create set called 'allowed' that is initially empty
    Allowed = sets:new(),
    %% For each ati in atis, compute the intersection of
    %% elements(ati).
    S1 = elements_intersection(G, ATIs),
    %% If any nodes in candidates are not in the computed
    %% intersection, add them to the allowed set and remove them from
    %% candidates
    S2 = sets:subtract(Candidates, S1), % nodes in candidates not in S1
    Allowed1 = sets:union(Allowed, S2), % add them to the allowed
    Candidates1 = sets:subtract(Candidates, S2), % remove from candidates
    %% Step c: for each ate in ates, compute the union of
    %% elements(ate).
    S3 = elements_union(G, ATEs),
    %% If any nodes in candidates are in the computed union, add them
    %% to the allowed set
    S4 = sets:intersection(Candidates1, S3), % nodes in candidates and in S3
    Allowed2 = sets:union(Allowed1, S4),
    %% Step d: return allowed set
    sets:to_list(Allowed2).

%% @doc for each `AT' in `ATs', compute the union of `elements(AT)'
elements_union(G, ATs) ->
    F = fun(E, Acc) ->
		Es = pm_pap:elements(G, E),
		sets:union(Acc, sets:from_list(Es))
	end,
    lists:foldl(F, sets:new(), ATs).
    
%% @doc for each `AT' in `ATs', compute the intersection of
%% `elements(AT)'
elements_intersection(_G, []) ->
    sets:new();
elements_intersection(G, [AT | Rest]) ->
    F = fun(E, Acc) ->
		Es = pm_pap:elements(G, E),
		sets:intersection(Acc, sets:from_list(Es))
	end,
    lists:foldl(F, sets:from_list(pm_pap:elements(G, AT)), Rest).
