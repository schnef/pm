-module(aap).

-include("pm.hrl").

-compile(export_all).

%% This module is used for experiments. Ignore!

powerset2(Lst) ->
    N = length(Lst),
    Max = trunc(math:pow(2, N)),
    powerset2(Lst, 0, N, Max).

powerset2(_, Max, _, Max) ->
    done;
powerset2(Lst, I, N, Max) -> % when I < Max ->
    %% The elements of the set are represented as bits
    Subset = [lists:nth(Pos + 1, Lst)
	      || Pos <- lists:seq(0, N - 1),
		 I band (1 bsl Pos) =/= 0],
    %% perform some actions on particular subset
    io:format("Subset ~p~n", [Subset]),
    powerset2(Lst, I + 1, N, Max).
%% powerset2(_, _, _, _) ->
%%     done.


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
    
    digraph:add_edge(G, U1, UA1),
    digraph:add_edge(G, UA1, UA11),
    digraph:add_edge(G, UA11, PC),
    digraph:add_edge(G, U2, UA2),
    digraph:add_edge(G, UA2, UA12),
    digraph:add_edge(G, UA12, PC),

    digraph:add_edge(G, O1, OA1),
    digraph:add_edge(G, OA1, OA11),
    digraph:add_edge(G, OA11, PC),
    digraph:add_edge(G, O2, OA2),
    digraph:add_edge(G, OA2, OA12),
    digraph:add_edge(G, OA12, PC),
    
    digraph_utils:topsort(G).


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

minimal() ->
    {ok, PC} = pm_pap:c_pc(#pc{value="pc"}).    

basic() ->
    {ok, PC} = pm_pap:c_pc(#pc{value="pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value="ua"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value="u"}, UA),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value="oa"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value="o"}, OA),
    [U] = pm_pap:users(UA),
    [O] = pm_pap:objects(OA),
    lists:sort([U, UA, O, OA]) =:= lists:sort(pm_pap:elements(PC)).

g() ->
    {ok, PC1} = pm_pap:c_pc(#pc{value="pc1"}),
    {ok, PC2} = pm_pap:c_pc(#pc{value="pc2"}),
    {ok, UA1} =  pm_pap:c_ua_in_pc(#ua{value="ua1"}, PC1),
    {ok, UA2} =  pm_pap:c_ua_in_ua(#ua{value="ua2"}, UA1),
    {ok, UA3} =  pm_pap:c_ua_in_ua(#ua{value="ua3"}, UA1),
    {ok, U1} =  pm_pap:c_u_in_ua(#u{value="u1"}, UA2),
    {ok, U2} =  pm_pap:c_u_in_ua(#u{value="u2"}, UA3),
    ok = pm_pap:c_ua_to_pc(UA3, PC2),

    {ok, PC3} = pm_pap:c_pc(#pc{value="pc3"}),
    {ok, OA21} =  pm_pap:c_oa_in_pc(#oa{value="oa21"}, PC1),
    {ok, OA20} =  pm_pap:c_oa_in_oa(#oa{value="oa20"}, OA21),
    {ok, O1} =  pm_pap:c_o_in_oa(#o{value="o1"}, OA20),
    {ok, O2} =  pm_pap:c_o_in_oa(#o{value="o2"}, OA20),
    ok = pm_pap:c_oa_to_pc(OA21, PC3),

    {ok, _Assoc1} = pm_pap:c_assoc(UA1, [#ar{id = 'r'}], OA21),
    %% {ok, _Assoc2} = pm_pap:c_assoc(UA2, [#ar{id = 'w'}], O1),
    {ok, _Assoc3} = pm_pap:c_assoc(UA3, [#ar{id = 'w'}], O2),

    %% {ok, PC2} = pm_pap:c_pc(#pc{value="pc2"}),
    %% {ok, UA4} =  pm_pap:c_ua_in_pc(#ua{value="ua4"}, PC2),
    %% {ok, UA5} =  pm_pap:c_ua_in_ua(#ua{value="ua5"}, UA4),
    %% {ok, UA6} =  pm_pap:c_ua_in_ua(#ua{value="ua6"}, UA4),
    %% ok = pm_pap:c_u_to_ua(U1, UA5),
    %% ok = pm_pap:c_u_to_ua(U2, UA6),

    %% {ok, PC3} = pm_pap:c_pc(#pc{}),
    %% {ok, UA7} =  pm_pap:c_ua_in_pc(#ua{}, PC3),
    %% {ok, UA8} =  pm_pap:c_ua_in_ua(#ua{}, UA7),
    %% {ok, UA9} =  pm_pap:c_ua_in_ua(#ua{}, UA8),
    %% {ok, U3} =  pm_pap:c_u_in_ua(#u{}, UA9),

    %% {ok, _Assoc4} = pm_pap:c_assoc(UA5, [#ar{id = 'c-u'}], O2),

    #{pc1 => PC1, pc2 => PC2, pc3 => PC3,
      ua1 => UA1, ua2 => UA2, ua3 => UA3, u1 => U1, u2 => U2,
      oa21 => OA21, oa20 => OA20, o1 => O1, o2 => O2
      %% pc2 => PC2, ua4 => UA4, ua5 => UA5, ua6 => UA6
     }.

g1() ->
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

g2() ->
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

tst1() ->
    pm_pap:clear(),
    {ok, G} = pm_pap:get_digraph(),
    M = g(),
    #{u1 := #u{id = U1}, u2 := #u{id = U2}, o1 := #o{id = O1}, o2 := #o{id = O2}} = M,
    pm:gv(G),
    [begin
	 %% io:format("find_border ~p~n", [pm_mell:find_border_at_priv_RESTRICTED(G, U)]),
	 io:format("calc_priv ~p~n", [sets:to_list(pm_mell:calc_priv_RESTRICTED(G, U ,O))])
     end || U <- [U1, U2],
	    O <- [O1, O2]].

tst2(Restricted) ->
    pm_pap:clear(),
    {ok, G} = pm_pap:get_digraph(),
    M = g(),
    #{u1 := #u{id = U1}, u2 := #u{id = U2}} = M,
    pm:gv(G),
    [io:format("accessible_objects ~p~n", [pm_mell:show_accessible_ats(G, U, Restricted)])
     || U <- [U1, U2]].

tst3(Restricted) ->
    pm_pap:clear(),
    {ok, G} = pm_pap:get_digraph(),
    M = g(),
    #{u1 := #u{id = U1}, u2 := #u{id = U2}} = M,
    pm:gv(G),
    [io:format("vis_initial_at ~p~n", [pm_mell:vis_initial_at(G, U, Restricted)])
     || U <- [U1, U2]].

tst4(Flag) ->
    pm_pap:clear(),
    {ok, G} = pm_pap:get_digraph(),
    M = g(),
    pm:gv(G),
    #{u1 := #u{id = U1}, u2 := #u{id = U2}, o1 := #o{id = O1}, o2 := #o{id = O2}} = M,
    [pm_mell:find_border_at_priv(G, U, Flag) || U <- [U1, U2]].

tst5(Flag) ->
    pm_pap:clear(),
    {ok, G} = pm_pap:get_digraph(),
    M = g1(),
    pm:gv(G),
    #{u1 := #u{id = U1}, u2 := #u{id = U2}, o1 := #o{id = O1}, o2 := #o{id = O2}} = M,
    [{OA, _} | _] = pm_mell:find_border_at_priv(G, U1, Flag),
    pm_mell:predecessor_at(G, U1, OA, Flag).
    
tst6(Flag) ->
    pm_pap:clear(),
    {ok, G} = pm_pap:get_digraph(),
    M = g2(),
    pm:gv(G),
    #{pc1 := #pc{id = PC1}, pc2 := #pc{id = PC2},
      ua1 := #ua{id = UA1}, ua2 := #ua{id = UA2}, ua3 := #ua{id = UA3},
      u1 := #u{id = U1}, u2 := #u{id = U2},
      oa21 := #oa{id = OA21}, oa20 := #oa{id = OA20},
      o1 := #o{id = O1}, o2 := #o{id = O2}} = M,
    pm_mell:successor_at(G, U1, OA20, Flag).
    
tst7(Flag) ->
    pm_pap:clear(),
    {ok, G} = pm_pap:get_digraph(),
    M = g2(),
    pm:gv(G),
    #{pc1 := #pc{id = PC1}, pc2 := #pc{id = PC2},
      ua1 := #ua{id = UA1}, ua2 := #ua{id = UA2}, ua3 := #ua{id = UA3},
      u1 := #u{id = U1}, u2 := #u{id = U2},
      oa21 := #oa{id = OA21}, oa20 := #oa{id = OA20},
      o1 := #o{id = O1}, o2 := #o{id = O2}} = M,
    pm_mell:find_orphan_objects(G, U2, Flag).

tst8() ->
    pm_pap:clear(),
    {ok, G} = pm_pap:get_digraph(),
    M = g(),
    pm:gv(G),
    #{ua1 := #ua{id = UA1}, ua2 := #ua{id = UA2}, ua3 := #ua{id = UA3},
      u1 := #u{id = U1}, u2 := #u{id = U2}} = M,
    [pm_mell:show_ua(G, U) || U <- [U1, U2]].

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

add_dependency(_G,L,L) ->
    ok;
add_dependency(G,L,D) ->
    digraph:add_vertex(G,D), % noop if dependency already added
    digraph:add_edge(G,D,L). % Dependencies represented as an edge D -> L

