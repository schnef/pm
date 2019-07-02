-module(aap).

-compile(export_all).

%% This module is used for experiments. Ignore!

run() ->
    PE = sets:from_list([1, 2, 3, 4, 5, 6, 7, 8, 9, 0]),
    ATIs = sets:from_list([2]),
    ATEs = sets:from_list([]),
    DisjRange = sets:union(ATIs, sets:subtract(PE, ATEs)),
    io:format("DisjRange ~p~n", [lists:sort(sets:to_list(DisjRange))]),
    ConjRange = sets:intersection(ATIs, sets:subtract(PE, ATEs)),
    io:format("ConjRange ~p~n", [lists:sort(sets:to_list(ConjRange))])
    .
    
run2() ->
    G = digraph:new([acyclic]),
    PC = digraph:add_vertex(G),
    UA11 = digraph:add_vertex(G),
    UA1 = digraph:add_vertex(G),
    U1 = digraph:add_vertex(G),
    UA12 = digraph:add_vertex(G),
    UA2 = digraph:add_vertex(G),
    U2 = digraph:add_vertex(G),

    OA11 = digraph:add_vertex(G),
    OA1 = digraph:add_vertex(G),
    O1 = digraph:add_vertex(G),
    OA12 = digraph:add_vertex(G),
    OA2 = digraph:add_vertex(G),
    O2 = digraph:add_vertex(G),
    
    digraph:add_edge(G, PC, UA11),
    digraph:add_edge(G, UA11, UA1),
    digraph:add_edge(G, UA1, U1),
    digraph:add_edge(G, PC, UA12),
    digraph:add_edge(G, UA12, UA2),
    digraph:add_edge(G, UA2, U2),

    digraph:add_edge(G, PC, OA11),
    digraph:add_edge(G, OA11, OA1),
    digraph:add_edge(G, OA1, O1),
    digraph:add_edge(G, PC, OA12),
    digraph:add_edge(G, OA12, OA2),
    digraph:add_edge(G, OA2, O2),

    sofs:digraph_to_family(G).

pe() ->
    Ext = [{pc, [ua11, ua12, oa11, oa12]},
	   {ua11, [ua1]},
	   {ua1, [u1]},
	   {u1, []},
	   {ua12, [ua2]},
	   {ua2, [u2]},
	   {u2, []},
	   {oa11, [oa1]},
	   {oa1, [o1]},
	   {o1, []},
	   {oa12, [oa2]},
	   {oa2, [o2]},
	   {o2, []}],
    sofs:from_external(Ext, [{atom,[atom]}]).
    
atis() ->
    Ext = [{oa1, [o1]},
	   {o1, []}],
    sofs:from_external(Ext, [{atom,[atom]}]).

ates() ->
    Ext = [{oa11, [oa1]},
    	   {oa1, [o1]},
    	   {o1, []}],
    sofs:from_external(Ext, [{atom,[atom]}]).

disj_range() ->
    ATIs = sofs:union_of_family(atis()),
    io:format("ATIs ~p~n", [ATIs]),
    ATEs = sofs:union_of_family(ates()),
    io:format("ATEs ~p~n", [ATEs]),
    PE = sofs:union_of_family(pe()),
    io:format("PE\PC~n~p~n", [PE]),
    sofs:union(ATIs, sofs:difference(PE, ATEs)).

conj_range() ->
    ATIs = sofs:intersection_of_family(atis()),
    io:format("ATIs ~p~n", [ATIs]),
    ATEs = sofs:intersection_of_family(ates()),
    io:format("ATEs ~p~n", [ATEs]),
    PE = sofs:union_of_family(pe()),
    io:format("PE\PC~n~p~n", [PE]),
    io:format("Diff PE, ATEs~n~p~n", [sofs:difference(PE, ATEs)]),
    sofs:intersection(ATIs, sofs:difference(PE, ATEs)).
