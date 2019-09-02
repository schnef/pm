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

%% =============================================================================
%% Digraph
%% =============================================================================

fig2() ->
    [{pc1, [ua11, ua12, oa21]},
     {ua11, [ua1]},
     {ua12, [ua1, ua2]},
     {ua1, [u1]},
     {ua2, [u2]},
     {oa21, [oa20]},
     {oa20, [oa1, oa2]},
     {oa1, [o1]},
     {oa2, [o2]}
    ].

%% Build graph with children pointing to their parent. This is what a
%% Policy Element Diagram should look like.
peg(L) ->
    F = fun(G, X, Y) ->
		digraph:add_edge(G, X, Y)
	end,
    g(L, F).
%% digraph_utils:topsort(G) will return
%% [u1,ua1,ua11,u2,ua2,ua12,o1,oa1,o2,oa2,oa20,oa21,pc1] for
%% peg(fig2()).

%% Build a graph with parents pointing to their children.
pegr(L) ->
    F = fun(G, X, Y) ->
		digraph:add_edge(G, Y, X)
	end,
    g(L, F).

%% digraph_utils:topsort(G) will return
%% [pc1,ua11,ua12,ua1,u1,ua2,u2,oa21,oa20,oa1,o1,oa2,o2] for
%% pegr(fig2()).

g(L, F) ->
    G = digraph:new([acyclic]),
    [begin
	 digraph:add_vertex(G, N),
	 [begin
	      digraph:add_vertex(G, C),
	      F(G, C, N)
	  end || C <- Cs]
     end || {N, Cs} <- L],
    G.

%% =============================================================================
%% Topological sort
%% https://rosettacode.org/wiki/Topological_sort#Erlang
%% =============================================================================

-define(LIBRARIES,
        [{des_system_lib,   [std, synopsys, std_cell_lib, des_system_lib, dw02, dw01, ramlib, ieee]},
         {dw01,             [ieee, dw01, dware, gtech]},
         {dw02,             [ieee, dw02, dware]},
         {dw03,             [std, synopsys, dware, dw03, dw02, dw01, ieee, gtech]},
         {dw04,             [dw04, ieee, dw01, dware, gtech]},
         {dw05,             [dw05, ieee, dware]},
         {dw06,             [dw06, ieee, dware]},
         {dw07,             [ieee, dware]},
         {dware,            [ieee, dware]},
         {gtech,            [ieee, gtech]},
         {ramlib,           [std, ieee]},
         {std_cell_lib,     [ieee, std_cell_lib]},
         {synopsys,         []}]).

-define(BAD_LIBRARIES,
        [{des_system_lib,   [std, synopsys, std_cell_lib, des_system_lib, dw02, dw01, ramlib, ieee]},
         {dw01,             [ieee, dw01, dw04, dware, gtech]},
         {dw02,             [ieee, dw02, dware]},
         {dw03,             [std, synopsys, dware, dw03, dw02, dw01, ieee, gtech]},
         {dw04,             [dw04, ieee, dw01, dware, gtech]},
         {dw05,             [dw05, ieee, dware]},
         {dw06,             [dw06, ieee, dware]},
         {dw07,             [ieee, dware]},
         {dware,            [ieee, dware]},
         {gtech,            [ieee, gtech]},
         {ramlib,           [std, ieee]},
         {std_cell_lib,     [ieee, std_cell_lib]},
         {synopsys,         []}]).

main() ->
    top_sort(?LIBRARIES),
    top_sort(?BAD_LIBRARIES).

top_sort(Library) ->
    G = digraph:new(),
    lists:foreach(fun ({L,Deps}) ->
                          digraph:add_vertex(G,L), % noop if library already added
                          lists:foreach(fun (D) ->
                                                add_dependency(G,L,D)
                                        end, Deps)
                  end, Library),
    T = digraph_utils:topsort(G),
    case T of
        false ->
            io:format("Unsortable contains circular dependencies:~n",[]),
            lists:foreach(fun (V) ->
                                  case digraph:get_short_cycle(G,V) of
                                      false ->
                                          ok;
                                      Vs ->
                                          print_path(Vs)
                                  end
                          end, digraph:vertices(G));
        _ ->
            print_path(T)
    end.

print_path(L) ->
    lists:foreach(fun (V) -> io:format("~s -> ",[V]) end,
		  lists:sublist(L,length(L)-1)),
    io:format("~s~n",[lists:last(L)]).

add_dependency(_G,_L,_L) ->
    ok;
add_dependency(G,L,D) ->
    digraph:add_vertex(G,D), % noop if dependency already added
    digraph:add_edge(G,D,L). % Dependencies represented as an edge D -> L

