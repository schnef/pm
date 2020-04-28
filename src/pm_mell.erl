-module(pm_mell).

%% TODO: Look into replacing lists with maps. There are several
%% functions that search potentially large lists and/or update a list
%% by replacing elements with new values. Maps may be more
%% efficient. Currently I stick with lists for consistency.

-include_lib("eunit/include/eunit.hrl").
-include("pm.hrl").

-export([find_border_at_priv_ANSI/2, find_border_at_priv_RESTRICTED/2,
	 calc_priv_ANSI/3, calc_priv_RESTRICTED/3,
	 access_ANSI/4, access_RESTRICTED/4,
	 show_accessible_ats_ANSI/2, show_accessible_ats_RESTRICTED/2,
	 vis_initial_at_ANSI/2, vis_initial_at_RESTRICTED/2,
	 predecessor_at_ANSI/3, predecessor_at_RESTRICTED/3,
	 successor_at_ANSI/3, successor_at_RESTRICTED/3,
	 find_orphan_objects_ANSI/2, find_orphan_objects_RESTRICTED/2,
	 show_ua/2, check_prohibitions/5]).

%% TODO: The cache currently does not work.
-define(USE_CACHE, false).

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
		      [{AT, PC, ARset}
		       || #association{ua = UA, at = AT, arset = ARset} <- Active,
			  PC <- find_pc_set(G, UA)];
		  false ->
		      [{AT, undefined, ARset}
		       || #association{at = AT, arset = ARset} <- Active]
	      end,

    %% For each edge, label the oa node with the access rights per
    %% applicable PC, conferred by the edge. Traverse the 'active' ua
    %% -> at edges and add the ARset of each association, per
    %% applicable PC, to the at, merging duplicate ARsets. At this
    %% point, we only use the ids of the ARsets and not the content of
    %% the sets yet. The result is the list of at's the ua's are
    %% pointing to with for each at a list of ARsets.
    F2 = fun({AT, PC, ARset}, Acc) ->
    		 case lists:keytake(AT, 1, Acc) of
		     {value, {_AT, PC_ARsets}, Acc1} ->
			 [{AT, case lists:keytake(PC, 1, PC_ARsets) of
				   {value, {_PC, ARsets}, PC_ARsets1} ->
				       [{PC, lists:merge(ARsets, [ARset])} | PC_ARsets1];
				   false ->
				       [{PC, [ARset]} | PC_ARsets]
			       end} | Acc1];
		     false ->
			 [{AT, [{PC, [ARset]}]} | Acc]
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
    F3 = fun({AT, PCs_ARsets}) ->
		 PCs_required = find_pc_set(G, AT),
		 PCs_ARsets1 = [{PC_required, ARsets}
				|| {PC_applicable, ARsets} <- PCs_ARsets,
				   PC_required <- PCs_required,
				   not Restricted orelse PC_applicable =:= PC_required],
		 case PCs_ARsets1 of
		     [] -> false;
		     _ -> {true, {AT, PCs_ARsets1}}
		 end
	 end,
    Active3 = lists:filtermap(F3, Active2),

    %% Now merge the ARsets per PC per AT.
    F4a = fun(ARset, Set2) ->
		  [#set{value = Set1}] = mnesia:dirty_read(arset, ARset),
		  sets:union(Set1, Set2)
	  end,
    F4b = fun({PC, ARsets}) ->
		  {PC, lists:foldl(F4a, sets:new(), ARsets)}
	  end,
    F4c = fun({AT, PC_ARsets}) ->
		  {AT, lists:map(F4b, PC_ARsets)}
	  end,
    lists:map(F4c, Active3).
    
find_pc_set(G, AT) ->
    find_pc_set(G, AT, ?USE_CACHE).

find_pc_set(G, AT, false) ->
    [PC || {pc, _} = PC <- digraph_utils:reachable_neighbours([AT], G)];
find_pc_set(_G, AT, true) ->
    {ok, PCs} = pm_pccache:find_pc_set(AT),
    PCs.

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
%% 1. Execute the `find_border_at_priv` function which will return a
%%    list with the *border* nodes that are of interest, i.e. looking for
%%    border nodes in the first step would duplicate the search already
%%    done by `find_border_at_priv`.
%% 2. From the target AT (previously oa), find all reachable nodes. Nodes
%%    can be o, oa and ua. NB: notice that we are not looking for border
%%    nodes, just nodes.
%% 3. During the same search, also accumulate a list of the required PC
%%    nodes.
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
calc_priv(G, U, AT_target, Restricted) ->
    %% TODO: Is it correct to assume that the target AT itself may be a
    %% border AT? I.e. use `digraph_utils:reachable' instead of
    %% `digraph_utils:reachable_neighbours'. Also note that the target
    %% AT can, by definition, only be a o, oa or ua and never a u.

    %% 1. execute find_border_oa_priv(u) (either the ANSI or NIST RESTRICTED
    %%    version) to find the set of 'oa border nodes'
    AT_border_nodes = find_border_at_priv(G, U, Restricted),

    %% 2. From oa, BFS to find all reachable oa border nodes. Pick up PCs along the way
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

    %% steps 3 thru 5
    ARsets = [ARset || {AT, Pairs} <- AT_border_nodes,
		       lists:member(AT, ATs),
		       {PC, ARset} <- Pairs,
		       not Restricted orelse lists:member(PC, PCs)],
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

-spec show_accessible_ats_ANSI(G, U) -> [{pm:id(), [AR]}] when
      G :: digraph:graph(),
      U :: pm:id(),
      AR :: atom().
%% @doc This function finds the unrestricted set of ats accessible to u.
show_accessible_ats_ANSI(G, U) ->
    show_accessible_ats(G, U, false).

-spec show_accessible_ats_RESTRICTED(G, U) -> [{pm:id(), [AR]}] when
      G :: digraph:graph(),
      U :: pm:id(),
      AR :: atom().
%% @doc This function finds the restricted set of ats accessible to u.
show_accessible_ats_RESTRICTED(G, U) ->
    show_accessible_ats(G, U, true).

-spec show_accessible_ats(G, U, Restricted) -> [{pm:id(), [AR]}] when
      G :: digraph:graph(),
      U :: pm:id(),
      Restricted :: boolean(),
      AR :: atom().
%% @doc This function finds the set of attributes accessible to u. This
%% would be used, for example, if the user wanted to do a keyword
%% search on all accessible attributes.
show_accessible_ats(G, U, Restricted) ->
    %% execute find_border_oa_priv(u) (either the ANSI or NIST
    %% RESTRICTED version) to find the set of 'oa border nodes'.
    AT_border_nodes = find_border_at_priv(G, U, Restricted),
    
    %% For each returned AT node, BFS backwards to find all relevant
    %% attributes. Label each discovered attribute with the
    %% accumulated access rights, from the acces right, PC node
    %% pairings from the AT border node (calculated by the
    %% find_border_at_priv method in step 1).

    %% Get all relevant ATs and pair with access right, PC from border AT
    AT_nodes1 = [{AT, PCs_ARsets}
		 || {AT_border_node, PCs_ARsets} <- AT_border_nodes,
		    AT <- digraph_utils:reaching([AT_border_node], G)],
    
    F1 = fun({AT, _PC_ARsets} = AT_node, Acc) ->
		 %% Select applicable ARsets
		 ARsets = select_arsets(G, AT_node, Restricted),
		 %% Merge the ARsets per AT
		 case ARsets of
		     [] -> Acc;
		     _ ->
			 case lists:keytake(AT, 1, Acc) of
			     {value, {_AT, ARset}, Acc1} ->
				 [{AT, lists:foldl(fun sets:union/2, ARset, ARsets)} | Acc1];
			     false ->
				 [{AT, lists:foldl(fun sets:union/2, sets:new(), ARsets)} | Acc]
			 end
		 end
	 end,
    AT_nodes2 = lists:foldl(F1, [], AT_nodes1),
    %% For each AT, turn sets into lists
    [{AT, sets:to_list(ARset)} || {AT, ARset} <- AT_nodes2].

%% For an attribute to be visible, the user must at least have the
%% VISIBLE_AR_REQUIRED access right
-define(VISIBLE_AR_REQUIRED, ['r']).

-spec vis_initial_at_ANSI(G, U) -> [{AT, ARs}] when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id(),
      ARs :: [atom()].
%% @doc Returns the initial set of AT nodes to display. THIS FOLLOWS
%% THE ANSI PRIVILEGE PM SPECIFICATION.
vis_initial_at_ANSI(G, U) ->
    vis_initial_at(G, U, false).

%% @doc Returns the initial set of AT nodes to display. THIS FOLLOWS
%% THE NIST RESTRICTED VERSION OF THE ANSI PM PRIVILEGE SPECIFICATION.
vis_initial_at_RESTRICTED(G, U) ->
    vis_initial_at(G, U, true).

%% @doc Returns the initial set of AT nodes to display.
%%
%% 1. execute find_border_oa_priv(U), restricted or unrestricted. to get
%%    set of border oa nodes and their access right, PC node pairings;
%% 2. Check for access right to visulize:
%%    - For the unrestricted version: Find out if any of the access
%%      right, PC node pairings allows for visulaizing the node, e.g. at
%%      least one of the ARsets has the "desired access rights" as a
%%      subset. That is, the AT node will be visualized if one of the PC,
%%      access right pairs allows this, even if other pairs do not allow
%%      this.
%%    - For the restricted version, select only those access right, PC
%%      node pairings for which the applicable and required PCs match and
%%      allow visualization if at least one of these pairs has the
%%      desired access rights as a subset.
%% 3. Return set of viewable nodes along with their access right, PC node
%%    pairings (from step 1). Note don't reduce the access right, PC node
%%    pairings based on the filtering done for step 2.
vis_initial_at(G, U, Restricted) ->
    AT_border_nodes = find_border_at_priv(G, U, Restricted),
    Desired_ARset = sets:from_list(?VISIBLE_AR_REQUIRED),
    F1a = fun(ARset) ->
		  sets:is_subset(Desired_ARset, ARset)
	  end,
    F1b = fun({AT, PC_ARsets} = AT_node) ->
    		  ARsets = select_arsets(G, AT_node, Restricted),
    		  lists:any(F1a, ARsets) andalso
		      {true, {AT, PC_ARsets}}
    	  end,
    lists:filtermap(F1b, AT_border_nodes).
    %% F1b = fun(AT_node) ->
    %% 		  ARsets = select_arsets(G, AT_node, Restricted),
    %% 		  lists:any(F1a, ARsets)
    %% 	  end,
    %% lists:filter(F1b, AT_border_nodes).

select_arsets(G, {AT, PC_ARsets}, Restricted) ->
    case Restricted of
	true ->
	    PCs_required = find_pc_set(G, AT),
	    [ARset || {PC, ARset} <- PC_ARsets,
		      lists:member(PC, PCs_required)];
	false ->
	    [ARset || {_PC, ARset} <- PC_ARsets]
    end.

-spec predecessor_at_ANSI(G, U, AT) -> [AT] when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id().
%% @doc This returns the next hierarchical level of AT nodes
predecessor_at_ANSI(G, U, AT) ->
    predecessor_at(G, U, AT, false).

-spec predecessor_at_RESTRICTED(G, U, AT) -> [AT] when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id().
%% @doc This returns the next hierarchical level of AT nodes
predecessor_at_RESTRICTED(G, U, AT) ->
    predecessor_at(G, U, AT, true).

%% @doc This returns the next hierarchical level of AT nodes to
%% display given a user and a target attribute (using the access graph
%% structure as a default way to explore the file structure). Note:
%% The AT input parameter is the entry that user, U, clicked. We
%% assume that U has the privilege to see AT in the directory tree if
%% this method is invoked.
predecessor_at(G, U, AT, Restricted) ->
    PCs_covered = lists:sort(find_pc_set(G, AT)),
    F1 = fun(X, {Avail, Not_avail}) ->
		 PCs_required = lists:sort(find_pc_set(G, X)),
		case PCs_covered =:= PCs_required of
		    true ->
			{[X | Avail], Not_avail};
		    false ->
			{Avail, [{X, PCs_required} | Not_avail]}
		end
	end,
    {Avail, Not_avail} = lists:foldl(F1, {[], []}, digraph:in_neighbours(G, AT)),
    case Not_avail of
	[] ->
	    Avail;
	_ ->
	    AT_border_nodes = find_border_at_priv(G, U, Restricted),
	    Desired_ARset = sets:from_list(?VISIBLE_AR_REQUIRED),
	    %% Build a list with all predecessors on the non available
	    %% list with their required PCs and the ARset, PC pairs
	    %% which they inhere from the border node.
	    L1 = [{X, PCs_required, PC_ARsets}
		  || {AT_border, PC_ARsets} <- AT_border_nodes,
		     Y <- digraph_utils:reachable([AT_border], G),
		     {X, PCs_required} <- Not_avail,
		     X =:= Y],
	    Avail ++ select_ats(L1, Desired_ARset)
    end.

-spec successor_at_ANSI(G, U, AT) -> [AT] when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id().
%% @doc This returns the set of valid parent nodes for oa given that
%% the user is u.
successor_at_ANSI(G, U, AT) ->
    successor_at(G, U, AT, false).

-spec successor_at_RESTRICTED(G, U, AT) ->[AT] when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id().
%% @doc This returns the set of valid parent nodes for oa given that
%% the user is u.
successor_at_RESTRICTED(G, U, AT) ->
    successor_at(G, U, AT, true).

%% @doc This returns the valid parent nodes of AT nodes to display
%% given a user and a target attribute (using the access graph
%% structure as a default way to explore the file structure). Note:
%% The AT input parameter is the entry that user, U, clicked. We
%% assume that U has the privilege to see AT in the directory tree if
%% this method is invoked.
successor_at(G, U, AT, Restricted) ->
    AT_border_nodes = find_border_at_priv(G, U, Restricted),
    Desired_ARset = sets:from_list(?VISIBLE_AR_REQUIRED),
    %% Build a list with all successors which are out neighbours and
    %% reaching to the border nodes, along with their required PCs
    %% and the ARset, PC pairs which they inhere from the border
    %% node. 
    Successor_nodes = [X || {Tag, _} = X <- digraph:out_neighbours(G, AT), Tag =/= pc],
    L = [{X, find_pc_set(G, X), PC_ARsets}
	 || {AT_border, PC_ARsets} <- AT_border_nodes,
	    X <- digraph_utils:reaching([AT_border], G),
	    lists:member(X, Successor_nodes)],
    select_ats(L, Desired_ARset).
    
%% @doc Select the ATs from the list L such that all required PCs are
%% covered by PC, ARset pairs and if so, the ARset does contain the
%% desired access rights. As a bonus, if the conditions are met, the
%% function returns a tuple {true, Value}, so a filtermap function can
%% be used.
select_ats(L, Desired_ARset) ->    
    F = fun({AT, PCs_required, PC_ARsets}) ->
		lists:all(fun(PC_required) ->
				  case lists:keyfind(PC_required, 1, PC_ARsets) of
				      {_PC, ARset} ->
					  sets:is_subset(Desired_ARset, ARset);
				      false ->
					  false
				  end
			  end, PCs_required) andalso
		    {true, AT}
	end,
    lists:filtermap(F, L).

-spec find_orphan_objects_ANSI(G, U) -> [AT] when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id().
%% @doc This returns the set of nodes that are accessible by u but not
%% reachable through the visualization interface
find_orphan_objects_ANSI(G, U) ->
    find_orphan_objects(G, U, false).

-spec find_orphan_objects_RESTRICTED(G, U) -> [AT] when
      G :: digraph:graph(),
      U :: pm:id(),
      AT :: pm:id().%% @doc This returns the set of nodes that are accessible by u but not
%% reachable through the visualization interface
find_orphan_objects_RESTRICTED(G, U) ->
    find_orphan_objects(G, U, true).

%% @doc UNFINISHED This returns the set of nodes that are accessible by u but not
%% reachable through the visualization interface because one or more
%% of the intervening object attribute nodes are not accessible. This
%% is not expected to be a normal occurrence (and does not occur in
%% any of our current example graphs).
%%
%% 1. Use algorithm vis_initial_oa(u) to find the set of visible border
%%    oa nodes (ANSI or RESTRICTED versions as appropriate).
%% 2. Create a hash table with the keys being node names and the each
%%    value being a set of privilege/PC pairings (note that duplicates
%%    will be automatically discarded)
%% 3. From each visible border oa node, x, BFS up (traversing the edges
%%    backwards) over the object DAG (i.e., don't traverse any ua->oa
%%    edges). Add each visited node to the hash table (if it isn't
%%    already there) and add x's privilege/PC pairings to the value of
%%    the visited node's entry.
%% 4. For each key in the hash table, x, execute find_pc_set(x) to find
%%    the required PCs. If the value field for x does not contain some
%%    privilege for which all required PCs are covered, then delete this
%%    key from the hash table. In more detail, the value set must have
%%    privilege/PC pairings with some privilege, p, where the associated
%%    PCs in the pairings with p must be a superset of the required
%%    PCs. The resulting hash table will contain only nodes that are
%%    accessible to u.
%% 5. For each key, x, in the reduced hash table that references an
%%    object (not an oa), BFS down attempting to reach a visible border
%%    oa node. However, only traverse nodes that are in the hash
%%    table. Note that nodes not in the hash table are either not
%%    accessible to u or will not provide a path to one of the visible
%%    border oa nodes. If the BFS terminates without reaching a visible
%%    border oa node, add x to the list of orphaned objects.
%% 6. Return list of orphaned objects.
%%
%% TODO: Unfinished implementation. Need some example PM to work with
%% but can't get one. 
find_orphan_objects(G, U, Restricted) ->
    Visible_border_nodes = vis_initial_at(G, U, Restricted),
    %% io:format("Visible_border_nodes ~p~n", [Visible_border_nodes]),
    %% Create the list of nodes, including the visible border nodes,
    %% with the accumulated PC, Access right pairs. Use reaching and
    %% not reaching neighbours to include the visible border nodes
    %% (AT) as well.
    L1 = [{X, PC_ARsets}
	  || {AT, PC_ARsets} <- Visible_border_nodes,
	     {Tag, _} = X <- digraph_utils:reaching([AT], G),
	     Tag =/= pc],
    %% io:format("L2 ~p~n", [L1]),
    
    %% The next two functions and the following fold will take the
    %% previous list and turn it into a map with the nodes as the keys
    %% and as the values the merged lists of PC, access right
    %% pairs. This is step 3, but with the value not being a set but a
    %% list instead, i.e. duplicate pairs are deleted.  F1 takes a
    %% pair PC, ARset and a list of PC, ARsets pairs and merge the
    %% first pair into the list. If the PC from the first pair is
    %% already in the pairs list, the ARset from the first pair is
    %% merged using a set union in the ARset found in the list
    %%
    %% NB: F1 and F2 are nearly identical, except for the function
    %% call to sets:union and lists:foldl. Functions like these are
    %% all over the program. Generalize?
    F1 = fun({PC, ARset1}, PC_ARsets) ->
		 case lists:keytake(PC, 1, PC_ARsets) of
		     {value, {_PC, ARset2}, PC_ARsets1} ->
			 [{PC, sets:union(ARset1, ARset2)} | PC_ARsets1];
		     false ->
			 [{PC, ARset1} | PC_ARsets]
		 end
	 end,
    F2 = fun({X, PC_ARsets1}, Acc) ->
    		 case lists:keytake(X, 1, Acc) of
		     {value, {_X, PC_ARsets2}, Acc1} ->
			 [{X, lists:foldl(F1, PC_ARsets2, PC_ARsets1)} | Acc1];
		     false ->
			 [{X, PC_ARsets1} | Acc]
		 end
	 end,
    L2 = lists:foldl(F2, [], L1),
    %% io:format("L2 ~p~n", [L2]),
    F4 = fun({X, PC_ARsets}) ->
		 PCs_required = find_pc_set(G, X),
		 ckeck_pc_arset(PC_ARsets, PCs_required)
	 end,
    L3 = lists:filter(F4, L2),
    io:format("L3 ~p~n", [L3]),
    Orphans = [X || {Tag, _} = X <- L3,
		    Tag =:= o orelse Tag =:= u],
    io:format("Candidates ~p~n", [Orphans]).

%% @doc This function checks for a) there is a pair PC, ARset for each
%% required PC and b) these pairs share some common access
%% rights. E.g. each required PC has for example read access.
ckeck_pc_arset(PC_ARsets, [PC_required | PCs_required]) ->
    case lists:keyfind(PC_required, 1, PC_ARsets) of
	{_PC_required, ARset} ->
	    ckeck_pc_arset(PC_ARsets, PCs_required, ARset);
	false ->
	    false
    end.

ckeck_pc_arset(_PC_ARsets, [], _ARset1) ->
    true;
ckeck_pc_arset(PC_ARsets, [PC_required | PCs_required], ARset1) ->
    case lists:keyfind(PC_required, 1, PC_ARsets) of
	{_PC_required, ARset2} ->
	    ARset3 = sets:intersection(ARset1, ARset2),
	    case sets:is_empty(ARset3) of
		true ->
		    false;
		false ->
		    ckeck_pc_arset(PC_ARsets, PCs_required, ARset3)
		end;
	false ->
	    false
    end.

-spec show_ua(G, UA) -> [UA] when
      G :: digraph:graph(),
      UA :: pm:id().
%% @doc This returns the set of descendants for node ua. Note, ua may
%% actual be a user instead of a user attribute.
show_ua(G, {Tag1, _} = UA1) when Tag1 =:= u; Tag1 =:= ua ->
    [UA2 || {Tag2, _} = UA2 <- digraph:out_neighbours(G, UA1),
	    Tag2 =/= pc].

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

%% @private 
%% @doc Check on equality of two sets. Two versions: one first
%% compares the sizes and if these match it checks ia one set is the
%% subset of the other, which must be true if sets are equal, The
%% second one checks set A to be a subset of B and vice versa. The
%% first version probably is faster since the sizes can be fetched
%% from the sets directly and the is_subset function is executed only
%% once.
sets_equal(A, B) ->
    sets:size(A) =:= sets:size(B) andalso sets:is_subset(A, B).
    %% sets:is_subset(A, B) andalso sets:is_subset(B, A).

%%%===================================================================
%%% Tests
%%%
%%% A set of really minimal tests. These tests don't test for the
%%% functions to be correct but are only used to check if they don't
%%% crash. Also, they are here as a reminder how to use the
%%% functions. For testing the functionality of the functions, more
%%% elaborate data sets must be developed.
%%%===================================================================

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
    {ok, G} = pm_pap:get_digraph(),
    M1 = pm1(), % Populate PM
    M2 = pm2(), % Populate PM
    M3 = pm3(), % Populate PM
    [tst_find_border_at_priv(G, M1),
     tst_calc_priv(G, M1),
     tst_show_accessible_ats(G, M1),
     tst_vis_initial_at(G, M1),
     tst_predecessor_at(G, M2),
     tst_successor_at(G, M3),
     tst_show_ua(G, M1)
    ].

%% @doc Recursive lists sort. Sort list and if an element of the list
%% is a key value pair with the value being a list, also sort that
%% list.
sort(L) ->    
    F = fun({K, [E | _] = V}) when is_tuple(E) -> {K, sort(V)};
	   ({K, V}) when is_list(V) -> {K, lists:sort(V)};
	   (E) -> E
	end,
    L1 = lists:keysort(1, L),
    lists:map(F, L1).

%% The following function(s) create simple PMs.
pm1() ->
    {ok, PC1} = pm_pap:c_pc(#pc{value="pc1"}),
    {ok, PC2} = pm_pap:c_pc(#pc{value="pc2"}),
    {ok, PC3} = pm_pap:c_pc(#pc{value="pc3"}),
    %% User DAG
    {ok, UA1} =  pm_pap:c_ua_in_pc(#ua{value="ua1"}, PC1),
    {ok, UA2} =  pm_pap:c_ua_in_ua(#ua{value="ua2"}, UA1),
    {ok, UA3} =  pm_pap:c_ua_in_ua(#ua{value="ua3"}, UA1),
    {ok, U1} =  pm_pap:c_u_in_ua(#u{value="u1"}, UA2),
    {ok, U2} =  pm_pap:c_u_in_ua(#u{value="u2"}, UA3),
    ok = pm_pap:c_ua_to_pc(UA3, PC2),
    %% Object DAG
    {ok, OA21} =  pm_pap:c_oa_in_pc(#oa{value="oa21"}, PC1),
    {ok, OA20} =  pm_pap:c_oa_in_oa(#oa{value="oa20"}, OA21),
    {ok, O1} =  pm_pap:c_o_in_oa(#o{value="o1"}, OA20),
    {ok, O2} =  pm_pap:c_o_in_oa(#o{value="o2"}, OA20),
    ok = pm_pap:c_oa_to_pc(OA21, PC3),
    %% Create associations for the access rights
    {ok, _Assoc1} = pm_pap:c_assoc(UA1, [#ar{id = 'r'}], OA21),
    {ok, _Assoc2} = pm_pap:c_assoc(UA3, [#ar{id = 'w'}], O2),
    
    #{pc1 => PC1, pc2 => PC2, pc3 => PC3,
      ua1 => UA1, ua2 => UA2, ua3 => UA3, u1 => U1, u2 => U2,
      oa21 => OA21, oa20 => OA20, o1 => O1, o2 => O2}.

pm2() ->
    {ok, PC1} = pm_pap:c_pc(#pc{value="pc1"}),
    {ok, PC2} = pm_pap:c_pc(#pc{value="pc2"}),
    {ok, UA1} =  pm_pap:c_ua_in_pc(#ua{value="ua1"}, PC1),
    {ok, UA2} =  pm_pap:c_ua_in_ua(#ua{value="ua2"}, UA1),
    ok = pm_pap:c_ua_to_pc(UA2, PC2),
    {ok, UA3} =  pm_pap:c_ua_in_ua(#ua{value="ua3"}, UA1),
    {ok, U1} =  pm_pap:c_u_in_ua(#u{value="u1"}, UA2),
    {ok, U2} =  pm_pap:c_u_in_ua(#u{value="u2"}, UA3),
    
    {ok, OA21} =  pm_pap:c_oa_in_pc(#oa{value="oa21"}, PC1),
    {ok, OA20} =  pm_pap:c_oa_in_oa(#oa{value="oa20"}, OA21),
    ok = pm_pap:c_oa_to_pc(OA20, PC2),
    {ok, O1} =  pm_pap:c_o_in_oa(#o{value="o1"}, OA20),
    {ok, O2} =  pm_pap:c_o_in_oa(#o{value="o2"}, OA20),

    {ok, _Assoc1} = pm_pap:c_assoc(UA1, [#ar{id = 'r'}], OA21),
    {ok, _Assoc2} = pm_pap:c_assoc(UA2, [#ar{id = 'r'}], OA20),
    {ok, _Assoc3} = pm_pap:c_assoc(UA3, [#ar{id = 'w'}], O2),
    #{pc1 => PC1, pc2 => PC2,
      ua1 => UA1, ua2 => UA2, ua3 => UA3, u1 => U1, u2 => U2,
      oa21 => OA21, oa20 => OA20, o1 => O1, o2 => O2
     }.

pm3() ->
    {ok, PC1} = pm_pap:c_pc(#pc{value="pc1"}),
    {ok, PC2} = pm_pap:c_pc(#pc{value="pc2"}),
    {ok, UA1} =  pm_pap:c_ua_in_pc(#ua{value="ua1"}, PC1),
    {ok, UA2} =  pm_pap:c_ua_in_ua(#ua{value="ua2"}, UA1),
    {ok, UA3} =  pm_pap:c_ua_in_ua(#ua{value="ua3"}, UA1),
    {ok, U1} =  pm_pap:c_u_in_ua(#u{value="u1"}, UA2),
    {ok, U2} =  pm_pap:c_u_in_ua(#u{value="u2"}, UA3),
    
    {ok, OA21} =  pm_pap:c_oa_in_pc(#oa{value="oa21"}, PC1),
    {ok, OA22} =  pm_pap:c_oa_in_pc(#oa{value="oa22"}, PC2),
    {ok, OA20} =  pm_pap:c_oa_in_oa(#oa{value="oa20"}, OA21),
    ok = pm_pap:c_oa_to_oa(OA20, OA22),
    {ok, O1} =  pm_pap:c_o_in_oa(#o{value="o1"}, OA20),
    {ok, O2} =  pm_pap:c_o_in_oa(#o{value="o2"}, OA20),

    {ok, _Assoc1} = pm_pap:c_assoc(UA1, [#ar{id = 'r'}], OA21),
    {ok, _Assoc2} = pm_pap:c_assoc(UA1, [#ar{id = 'r'}], OA22),
    {ok, _Assoc3} = pm_pap:c_assoc(UA3, [#ar{id = 'w'}], O2),
    #{pc1 => PC1, pc2 => PC2,
      ua1 => UA1, ua2 => UA2, ua3 => UA3, u1 => U1, u2 => U2,
      oa22 => OA22, oa21 => OA21, oa20 => OA20, o1 => O1, o2 => O2
     }.

tst_find_border_at_priv(G, M) ->
    #{pc1 := #pc{id = PC1}, pc3 := #pc{id = PC3}, 
      u1 := #u{id = U1}, u2 := #u{id = U2},
      oa21 := #oa{id = OA21}, o2 := #o{id = O2}} = M,
    [?_assertEqual(sort([{OA21, [{PC1, sets:from_list([r])}, {PC3, sets:from_list([r])}]}]),
		   sort(pm_mell:find_border_at_priv_ANSI(G, U1))),
     ?_assertEqual(sort([{OA21, [{PC1, sets:from_list([r])}, {PC3, sets:from_list([r])}]},
			 {O2, [{PC1, sets:from_list([w])}, {PC3, sets:from_list([w])}]}]),
		   sort(pm_mell:find_border_at_priv_ANSI(G, U2))),
     ?_assertEqual([{OA21, [{PC1, sets:from_list([r])}]}],
		   sort(pm_mell:find_border_at_priv_RESTRICTED(G, U1))),
     ?_assertEqual(sort([{OA21, [{PC1, sets:from_list([r])}]},
			 {O2, [{PC1, sets:from_list([w])}]}]),
		   sort(pm_mell:find_border_at_priv_RESTRICTED(G, U2)))
     ].

tst_calc_priv(G, M) ->
    #{u1 := #u{id = U1}, u2 := #u{id = U2}, o1 := #o{id = O1}, o2 := #o{id = O2}} = M,
    [?_assertEqual([r],
		   lists:sort(sets:to_list(pm_mell:calc_priv_ANSI(G, U1 ,O1)))),
     ?_assertEqual([r],
		   lists:sort(sets:to_list(pm_mell:calc_priv_ANSI(G, U1 ,O2)))),
     ?_assertEqual([r],
		   lists:sort(sets:to_list(pm_mell:calc_priv_ANSI(G, U2 ,O1)))),
     ?_assertEqual([r,w],
		   lists:sort(sets:to_list(pm_mell:calc_priv_ANSI(G, U2 ,O2)))),
     ?_assertEqual([r],
		   lists:sort(sets:to_list(pm_mell:calc_priv_RESTRICTED(G, U1 ,O1)))),
     ?_assertEqual([r],
		   lists:sort(sets:to_list(pm_mell:calc_priv_RESTRICTED(G, U1 ,O2)))),
     ?_assertEqual([r],
		   lists:sort(sets:to_list(pm_mell:calc_priv_RESTRICTED(G, U2 ,O1)))),
     ?_assertEqual([r,w],
		   lists:sort(sets:to_list(pm_mell:calc_priv_RESTRICTED(G, U2 ,O2))))
    ].

tst_show_accessible_ats(G, M) ->
    #{u1 := #u{id = U1}, u2 := #u{id = U2},
      oa20 := #oa{id = OA20}, oa21 := #oa{id = OA21},
      o1 := #o{id = O1}, o2 := #o{id = O2}} = M,
    [?_assertEqual(sort([{OA21, [r]}, {OA20, [r]}, {O1, [r]}, {O2, [r]}]),
		   sort(pm_mell:show_accessible_ats_ANSI(G, U1))),
     ?_assertEqual(sort([{OA21, [r]}, {OA20, [r]}, {O1, [r]}, {O2, [r,w]}]),
		   sort(pm_mell:show_accessible_ats_ANSI(G, U2))),
     ?_assertEqual(sort([{OA21, [r]}, {OA20, [r]}, {O1, [r]}, {O2, [r]}]),
		   sort(pm_mell:show_accessible_ats_RESTRICTED(G, U1))),
     ?_assertEqual(sort([{OA21, [r]}, {OA20, [r]}, {O1, [r]}, {O2, [r, w]}]),
		   sort(pm_mell:show_accessible_ats_RESTRICTED(G, U2)))
    ].

tst_vis_initial_at(G, M) ->
    #{pc1 := #pc{id = PC1}, pc3 := #pc{id = PC3}, 
      u1 := #u{id = U1}, u2 := #u{id = U2},
      oa21 := #oa{id = OA21}} = M,
    R_arset = sets:from_list([r]),
    [?_assertEqual(sort([{OA21, [{PC1, R_arset}, {PC3, R_arset}]}]),
		   sort(pm_mell:vis_initial_at_ANSI(G, U1))),
     ?_assertEqual(sort([{OA21, [{PC1, R_arset}, {PC3, R_arset}]}]),
		   sort(pm_mell:vis_initial_at_ANSI(G, U2))),
     ?_assertEqual(sort([{OA21, [{PC1, R_arset}]}]),
		   sort(pm_mell:vis_initial_at_RESTRICTED(G, U1))),
     ?_assertEqual(sort([{OA21, [{PC1, R_arset}]}]),
		   sort(pm_mell:vis_initial_at_RESTRICTED(G, U2)))].

tst_predecessor_at(G, M) ->
    #{u1 := #u{id = U1}, u2 := #u{id = U2},
      oa20 := #oa{id = OA20}, oa21 := #oa{id = OA21}} = M,
    [?_assertEqual(lists:sort([OA20]),
		   lists:sort(pm_mell:predecessor_at_ANSI(G, U1, OA21))),
     ?_assertEqual([],
		   lists:sort(pm_mell:predecessor_at_ANSI(G, U2, OA21))),
     ?_assertEqual(lists:sort([OA20]),
		   lists:sort(pm_mell:predecessor_at_RESTRICTED(G, U1, OA21))),
     ?_assertEqual([],
		   lists:sort(pm_mell:predecessor_at_RESTRICTED(G, U2, OA21)))].

tst_successor_at(G, M) ->
    #{u1 := #u{id = U1}, u2 := #u{id = U2},
      oa20 := #oa{id = OA20}, oa21 := #oa{id = OA21}, oa22 := #oa{id = OA22}} = M,
    [?_assertEqual(lists:sort([OA21, OA22]),
		   lists:sort(pm_mell:successor_at_ANSI(G, U1, OA20))),
     ?_assertEqual(lists:sort([OA21, OA22]),
		   lists:sort(pm_mell:successor_at_ANSI(G, U2, OA20))),
     ?_assertEqual(lists:sort([OA21]),
		   lists:sort(pm_mell:successor_at_RESTRICTED(G, U1, OA20))),
     ?_assertEqual(lists:sort([OA21]),
		   lists:sort(pm_mell:successor_at_RESTRICTED(G, U2, OA20)))].

tst_show_ua(G, M) ->
    #{u1 := #u{id = U1}, u2 := #u{id = U2},
      ua2 := #ua{id = UA2}, ua3 := #ua{id = UA3}} = M,
    [?_assertEqual([UA2],
		   pm_mell:show_ua(G, U1)),
     ?_assertEqual([UA3],
		   pm_mell:show_ua(G, U2))].

-endif.
