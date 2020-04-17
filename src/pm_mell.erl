-module(pm_mell).

-include("pm.hrl").

-compile(export_all).

-export([gv/1]).
-export([find_border_at_priv_ANSI/2, find_border_at_priv_RESTRICTED/2,
	 calc_priv_ANSI/3, calc_priv_RESTRICTED/3,
	 access_ANSI/4, access_RESTRICTED/4,
	 show_accessible_objects/3, vis_initial_oa_ANSI/2,
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
    {PEs1, _, _, _, _, _} = lists:foldl(F1, {[], 1, 1, 1, 1, 1}, PEs),
    M = maps:from_list([{Id, Name} || {Id, Name, _Label} <- PEs1]),
    F2 = fun({_Id, Name, undefined}) ->
		 io_lib:format("  ~s [label=\"<~s>\" shape=plaintext]", [Name, Name]);
	    ({_Id, Name, Label}) ->
		 io_lib:format("  ~s [label=\"~s\" shape=plaintext]", [Name, Label])
	 end,
    Ns = lists:map(F2, PEs1),

    PCs = [io_lib:format("  ~s -> ~s", [maps:get(Id2, M), Name])
	   || {{pc, _} = Id1, Name, _Label} <- PEs1,
	      Id2 <- digraph:in_neighbours(G, Id1)],

    Us = [io_lib:format("  ~s -> ~s", [maps:get(Id2, M), Name])
	  || {{ua, _} = Id1, Name, _Label} <- PEs1,
	     {Tag, _} = Id2 <- digraph:in_neighbours(G, Id1),
	     Tag =:= u orelse Tag =:= ua],

    Os = [io_lib:format("  ~s -> ~s", [maps:get(Id2, M), Name])
	  || {{oa, _} = Id1, Name, _Label} <- PEs1,
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
    Assocs = lists:foldl(F3, [], PEs1),
    As = [io_lib:format("  ~s -> ~s [label=\"~p\", constraint=false, splines=curved, style=dashed]",
			[maps:get(UA, M), maps:get(AT, M), sets:to_list(ARs)])
	  || #association{ua = UA, at = AT, arset = ARset} <- Assocs,
	     #set{value = ARs} <- mnesia:dirty_read(arset, ARset)],
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

-spec find_border_at_priv_ANSI(G, U) -> ATnodes when
      G :: digraph:graph(),
      U :: pm:id(),
      ATnodes :: [{AT_id, [{PC_id, [AR_id]}]}],
      AT_id :: pm:id(),
      AR_id :: pm:id(),
      PC_id :: pm:id().
%% @doc This function returns all oa _and_ ua nodes that are
%% successors of ua nodes through association (for ua nodes reachable
%% from u). THIS FOLLOWS THE ANSI PRIVILEGE PM SPECIFICATION. It
%% labels each returned node with the reachable PC nodes paired with
%% the access rights conferred by the ua->oa edges.
find_border_at_priv_ANSI(G, U) ->
    find_border_at_priv(G, U, false).

-spec find_border_at_priv_RESTRICTED(G, U) -> ATnodes when
      G :: digraph:graph(),
      U :: pm:id(),
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
find_border_at_priv_RESTRICTED(G, U) ->
    find_border_at_priv(G, U, true).

find_border_at_priv(G, U, Restricted) ->
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

-spec calc_priv_ANSI(G, U ,AT) -> ARs when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id(),
      ARs :: [atom()].
%% @doc This returns all privileges that u has on oa, using a
%% unrestricted match for applicable and required PCs,
calc_priv_ANSI(G, U ,AT_target) ->
    calc_priv(G, U ,AT_target, false).

-spec calc_priv_RESTRICTED(G, U ,AT) -> ARs when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id(),
      ARs :: [atom()].
%% @doc This returns all privileges that u has on oa, using a
%% restricted match for applicable and required PCs,
calc_priv_RESTRICTED(G, U ,AT_target) ->
    calc_priv(G, U ,AT_target, true).

-spec calc_priv(G, U ,AT, Restricted) -> ARs when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id(),
      Restricted :: boolean(),
      ARs :: [atom()].
%% @doc This returns all privileges that u has on oa. 
%%
%% The following is according to the document by Mell. The
%% implementation is however different as pointed out
%% eblow. 
%%
%% complexity: O(n2+nm) but pseudo-linear
%%
%% 1. execute find_border_oa_priv(u) (either the ANSI or NIST RESTRICTED
%%    version) to find the set of 'oa border nodes'
%% 2. From oa, BFS to find all reachable oa border nodes
%% 3. Label oa with all access right, PC node pairings on the reachable
%%    oa border nodes (calculated by the find_border_oa_priv method in
%%    step 1). For the labelling, use a hash table with the access right
%%    as the key and a set of PC nodes as the values (note that this will
%%    automatically consolidate duplicates)
%% 4. BFS from oa to find a set of required PC nodes. 
%% 5. Traverse the list of access right keys in the hash table. For each
%%    access right key, x, extract the value y. If the set of required PC
%%    nodes from step 4 is a subset of y, then add x to the set of
%%    approved access rights.
%% 6. Return the set of approved access rights.
%%
%% Because privelleges not only apply to object attributes, but also to
%% objects which are considered object attributes and to user attributes,
%% the implementation extends the algorithm to include these as well.
%%
%% *border* attribuets are defined as the set of endpoints of the
%% associations of the ua to at edges that are labeled with access rights
%% (ars).
%%
%% The order in which the steps are executed in the current
%% implementation differs from those described above. The order is now:
%%
%% 1. From the target AT (previously oa), find all reachable nodes. Nodes
%%    can be o, oa and ua. NB: notice that we are not looking for border
%%    nodes, just nodes.
%% 2. During the same search, also accumulate a list of the required PC
%%    nodes.
%% 3. Now execute the `find_border_at_priv` function which will return a
%%    list with the *border* nodes that are of interest, i.e. looking for
%%    border nodes in the first step would duplicate the search already
%%    done by `find_border_at_priv`.
%% 4. Now, for each of the border nodes found in step 3:
%%     1. Select the border ATs which are reachable from the target AT as
%%        found in step 1,
%%     2. For each selected border AT, fetch the access right, PC node
%%        pairings in which the PC is a memeber of the required PCs from
%%        step 2. The result of this step is a a list of sets, each set
%%        containing the access rights defined by the applicable
%%        associations.
%%        NB: when using the unrestricted `find_border_at_priv_ANSI`
%%        function, the applicable PCs returned by that function are
%%        `undefined` which is handled by the `PC =:= undefined orelse
%%        lists:member(PC, PCs)` guard.
%% 5. Last, merge the list of access right sets and return the result as
%%    a set of access rights the user holds over the target AT.
%%
%% The current goal is to get something working, hopefully correctly.
calc_priv(G, U ,AT_target, Restricted) ->
    %% TODO: Is it correct to assume that the target AT itself may be a
    %% border AT? I.e. use `digraph_utils:reachable' instead of
    %% `digraph_utils:reachable_neighbours'. Also note that the target
    %% AT can, by definition, only be a o, oa or ua and never a u.
    F1 = fun({ua, _} = X, {Xs, Ys}) ->
		 {[X | Xs], Ys};
	    ({o, _} = X, {Xs, Ys}) ->
		 {[X | Xs], Ys};
	    ({oa, _} = X, {Xs, Ys}) ->
		 {[X | Xs], Ys};
	    ({pc, _} = Y, {Xs, Ys}) ->
		 {Xs, [Y | Ys]}
	 end,
    {ATs, PCs} = lists:foldl(F1, {[], []}, digraph_utils:reachable([AT_target], G)),
    %% io:format("++ pcs ~p~n", [PCs]),
    %% io:format("++ ats ~p~n", [ATs]),
    AT_nodes = find_border_at_priv(G, U, Restricted),
    %% io:format("++ at_nodes ~p~n", [AT_nodes1]),
    ARsets = [ARset || {AT, Pairs} <- AT_nodes,
		       lists:member(AT, ATs),
		       {PC, ARset} <- Pairs,
		       PC =:= undefined orelse lists:member(PC, PCs)],
    F2 = fun(S1, S2) ->
		 sets:union(S1, S2)
	 end,
    lists:foldl(F2, sets:new(), ARsets).

-spec access_ANSI(G, U, AR, AT) -> boolean() when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id(),
      AR :: atom().
%% @doc This function determines whether or not U has privilege AR on AT, using a
%% unrestricted match for applicable and required PCs.
access_ANSI(G, U, AR, AT) ->
    access(G, U, AR, AT, false).

-spec access_RESTRICTED(G, U, AR, AT) -> boolean() when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id(),
      AR :: atom().
%% @doc This function determines whether or not U has privilege AR on AT, using a
%% restricted match for applicable and required PCs.
access_RESTRICTED(G, U, AR, AT) ->
    access(G, U, AR, AT, true).

-spec access(G, U, AR, AT, Restricted) -> boolean() when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id(),
      Restricted :: boolean(),
      AR :: atom().
%% @doc This function determines whether or not U has privilege AR on AT.
access(G, U, AR, AT, Restricted) ->
    ARset = calc_priv(G, U ,AT, Restricted),
    sets:is_element(AR, ARset).

-spec show_accessible_objects(G, U, Restricted) -> [{pm:id(), [AR]}] when
      G :: digraph:graph(),
      U :: pm:id(),
      Restricted :: boolean(),
      AR :: atom().
%% @doc This function finds the set of objects accessible to u. This
%% would be used, for example, if the user wanted to do a keyword
%% search on all accessible object.
show_accessible_objects(G, U, Restricted) ->
    AT_nodes = find_border_at_priv(G, U, Restricted),
    %% For all border ATs from the AT_nodes list, get the set of
    %% ARs. In case this is a restricted search, only return the ARs
    %% set if the applicable PC matches a border AT restricted PC.
    L1 = relevant_border_nodes(G, AT_nodes, Restricted),
    %% At this point, we have a list L with all border ATs, paired
    %% with a set of ARs the user U holds. Now we have to look for the
    %% relevant Os and Us.
    L2 = [{UO, ARset} || {AT, ARset} <- L1,
			 {Tag, _} = UO <- digraph_utils:reaching([AT], G),
			 Tag =:= u orelse Tag =:= o],
    %% We now have a list of U and O nodes with for every node the set
    %% of ARs coming from the ATs. Now merge the ARsets per node.
    F2 = fun({UO, ARset}, Acc) ->
		 Set = case Acc of
			   #{UO := Set1} ->
			       sets:union(Set1, ARset);
			   _ ->
			       ARset
		       end,
		 Acc#{UO => Set}
	 end,
    M = lists:foldl(F2, #{}, L2),
    %% Return the U and O nodes found with for each node the ARs as a
    %% list.
    [{UO, sets:to_list(ARset)} || {UO, ARset} <- maps:to_list(M)].

%% @doc This function takes a list of potential border nodes of
%% interest and will turn that list in a list which contains the
%% relevant border nodes as tuples {AT, ARset}. When restricted, only
%% the border nodes are returned where the applicable and required PCs
%% match.
relevant_border_nodes(G, AT_nodes, Restricted) ->
    case Restricted of
	true ->
		F1 = fun({pc, _}) -> true;
			(_) -> false
		     end,
	    [{AT, ARset}
	     || {AT, Pairs} <- AT_nodes,
		PC1 <- lists:filter(F1, digraph_utils:reachable_neighbours([AT], G)),
		{PC2, ARset} <- Pairs,
		PC1 =:= PC2];
	false ->
	    [{AT, ARset}
	     || {AT, Pairs} <- AT_nodes,
		{_PC, ARset} <- Pairs]
    end.

%% For an attribute to be visible, the user must at least have the
%% VISIBLE_AR_REQUIRED access right
-define(VISIBLE_AR_REQUIRED, 'r').

%% @doc Returns the initial set of oa nodes to display when a user
%% wants to explore their files (and use the access graph structure as
%% a default way to explore them). THIS FOLLOWS THE ANSI PRIVILEGE PM
%% SPECIFICATION.
vis_initial_oa_ANSI(G, U) ->
    vis_initial_oa(G, U, false).

%% @doc Returns the initial set of oa nodes to display when a user
%% wants to explore their files (and use the access graph structure as
%% a default way to explore them). THIS FOLLOWS THE NIST RESTRICTED
%% VERSION OF THE ANSI PM PRIVILEGE SPECIFICATION.
vis_initial_oa_RESTRICTED(G, U) ->
    vis_initial_oa(G, U, true).

vis_initial_oa(G, U, Restricted) ->
    AT_nodes = find_border_at_priv(G, U, Restricted),
    L1 = relevant_border_nodes(G, AT_nodes, Restricted),
    %% For a node to be visible, all AR sets for each AT must contain
    %% the required AR, i.e. each AR set must at least contain the 'r'
    %% (read) access right. If for some {AT, ARset} this isn't the
    %% case, the AT should not be visible.
    %%
    %% The following function adds an AT to the shown map if the ARset
    %% contains the minimum required AR. If the ARset doesn't contain
    %% the minimum AR, it will add the AT to the ignored ATs and the
    %% AT is removed--if present--from the shown map.
    F2 = fun({AT, ARset}, {Shown, Ignored}) ->
		 case lists:member(AT, Ignored) of
		     true ->
			 {Shown, Ignored};
		     false ->
			 case sets:is_element(?VISIBLE_AR_REQUIRED, ARset) of
			     true ->
				 Set = case Shown of
					   #{AT := Set1} ->
					       sets:union(Set1, ARset);
					   _ ->
					       ARset
				       end,
				 {Shown#{AT => Set}, Ignored};
			     false ->
				 {maps:remove(AT, Shown), [AT | Ignored]}
			 end
		 end
	 end,
    {M, _} = lists:foldl(F2, {#{}, []}, L1),
    [{AT, sets:to_list(ARset)} || {AT, ARset} <- maps:to_list(M)].

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