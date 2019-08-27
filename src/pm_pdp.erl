-module(pm_pdp).

%%% @doc The PDP is implemented as a gen_server since we have to
%%% serialize the decision making process.

%% TODO: Review the way prohibitions work with determining the type of
%% attribute that should be checked. I.e., if the attribute that is
%% being accessed is an object or object attribute, we set the AT_type
%% to oa and use that value to select the table for looking up the
%% attributes. Maybe, the type should be set when the prohibition is
%% defined in the PIP.

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include("pm.hrl").

%% API
-export([start_link/0, stop/0, privilege/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {g}).

%%%===================================================================
%%% API
%%%===================================================================

privilege(_U, [], _AT) ->
    {error, badarg};
privilege(U, ARl, AT) ->
    gen_server:call(?SERVER, {privilege, U, ARl, AT}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, G} = pm_pap:get_digraph(),
    {ok, #state{g = G}}.

%% @private
handle_call({privilege, U, ARl, AT}, _From, #state{g = G} = State) ->
    Reply = privilege(G, U, ARl, AT),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

privilege(G, U, ARl, #ua{id = UA_id}) ->
    case mnesia:dirty_read(pe, UA_id) of
	[#pe{vertex = Vua}] ->
	    privilege(G, U, ARl, Vua);
	[] ->
	    {error, not_found}
    end;
privilege(G, U, ARl, #o{id = O_id}) ->
    case mnesia:dirty_read(pe, O_id) of
	[#pe{vertex = Vo}] ->
	    privilege(G, U, ARl, Vo);
	[] ->
	    {error, not_found}
    end;
privilege(G, U, ARl, #oa{id = OA_id}) ->
    case mnesia:dirty_read(pe, OA_id) of
	[#pe{vertex = Voa}] ->
	    privilege(G, U, ARl, Voa);
	[] ->
	    {error, not_found}
    end;
privilege(G, #u{id = U_id}, ARl, Vpe) ->
    case mnesia:dirty_read(pe, U_id) of
	[#pe{vertex = Vu}] ->
	    ARs = sets:from_list([AR_id || #ar{id = AR_id} <- ARl]),
	    U_deny_disj = mnesia:dirty_read(u_deny_disj, U_id),
	    U_deny_conj = mnesia:dirty_read(u_deny_conj, U_id),
	    case prohibited(G, U_deny_disj, U_deny_conj, Vpe, ARs) of
		true ->
		    deny;
		false ->
		    privilege(G, digraph:out_edges(G, Vu), ARs, Vpe, [], [])
	    end;
	[] ->
	    {error, not_found}
    end.

privilege(_G, [], _ARs, _Vpe, _Vua_visited, _AT_ignore) ->
    deny;
privilege(G, [Edge | Rest], ARs, Vpe, Vua_visited, AT_ignore) ->
    {_Edge, _V, Vua, UA_id} = digraph:edge(G, Edge),
    Result = case lists:member(Vua, Vua_visited) of
    		 false ->
		     UA_deny_disj = mnesia:dirty_read(ua_deny_disj, UA_id),
		     UA_deny_conj = mnesia:dirty_read(ua_deny_conj, UA_id),
		     case prohibited(G, UA_deny_disj, UA_deny_conj, Vpe, ARs) of
    			 true ->
    			     deny;
    			 false ->
			     Assocs = mnesia:dirty_read(assoc, UA_id),
    			     assocs(G, Assocs, ARs, Vpe, AT_ignore)
    		     end;
    		 true ->
    		     {ARs, AT_ignore}
    	     end,
    case Result of
    	{ARs_remain, AT_ignore1} ->	
    	    %% TODO: we go down to the pc and don't stop at the last ua
    	    %% before finding out we cannot go down any further. Change
    	    %% stop criterium?
    	    Edges = digraph:out_edges(G, Vua),
    	    %% TODO: Can the construct Vuas ++ Rest give trouble for tail recursion?
    	    privilege(G, Edges ++ Rest, ARs_remain, Vpe, [Vua | Vua_visited], AT_ignore1);
    	_ -> % the result is either grant or deny
    	    Result
    end.

assocs(_G, [], ARs, _Vpe, AT_ignore) ->
    {ARs, AT_ignore};
assocs(G, [#assoc{b = ARs_id, c = AT_id} | Assocs], ARs, Vpe, AT_ignore) ->
    case lists:member(AT_id, AT_ignore) of
	false ->
	    [#set{value = ARs_assoc}] = mnesia:dirty_read(arset, ARs_id),
	    case sets:subtract(ARs, ARs_assoc) of
		ARs -> % Association has no matching AR's
		    assocs(G, Assocs, ARs, Vpe, AT_ignore);
		ARs1 -> % Partial or full match
		    [#pe{vertex = Vat}] = mnesia:dirty_read(pe, AT_id),
		    case Vpe =:= Vat orelse digraph:get_path(G, Vpe, Vat) of
			false -> % Vpe is not contained by Vat
			    assocs(G, Assocs, ARs, Vpe, [AT_id | AT_ignore]);
			_ -> % There is a path so Vpe is contained by Vat
			    %% TODO: Can we use the path found to
			    %% build a list with vertices which
			    %% contain Vpe. Next time we have to look
			    %% for a path, we can first check to see
			    %% if Vat is already in the list of
			    %% verices, which maybe improves
			    %% efficiency?
			    case sets:size(ARs1) of
				0 -> % Matched all AR's, so we are done
				    grant;
				_ -> % Look for more associations for the remaining AR's
				    assocs(G, Assocs, ARs1, Vpe, AT_ignore)
			    end
		    end
	    end;
	true ->
	    ?LOG_DEBUG("Ignored ~p~n", [AT_id]),
	    assocs(G, Assocs, ARs, Vpe, AT_ignore)
    end.

%% Is called as prohibited(G, UA_deny_conj, UA_deny_disj, Vpe, ARs, AT_type)
prohibited(_G, [], [], _Vpe, _ARs) ->
    false;
prohibited(G, Disj, Conj, Vpe, ARs) ->
    case prohib(G, disjunctive, Disj, Vpe, ARs) of
	false ->
	    prohib(G, conjunctive, Conj, Vpe, ARs);
	true ->
	    true
    end.

prohib(_G, _Type, [], _Vpe, _ARs) ->
    false;
prohib(G, Type, [#prohib{b = ARs_id, c = ATI_id, d = ATE_id} | Rest], Vpe, ARs) ->    
    [#set{value = ARs_prohib}] = mnesia:dirty_read(arset, ARs_id),
    Result = case sets:is_disjoint(ARs, ARs_prohib) of
		 false ->
		     [#set{value = ATIs}] = mnesia:dirty_read(atiset, ATI_id),
		     [#set{value = ATEs}] = mnesia:dirty_read(ateset, ATE_id),
		     case Type of
			 disjunctive ->
			     disj_prohib(G, ATIs, ATEs, Vpe);
			 conjunctive ->
			     conj_prohib(G, ATIs, ATEs, Vpe)
		     end;
		 true ->
		     false
	     end,
    case Result of
	true ->
	    true;
	false ->
	    prohib(G, Type, Rest, Vpe, ARs)
    end.

%% TRUE -> PROHIBITED = NOT ALLOWED
disj_prohib(_G, _ATIs, [], _Vpe) ->
    true; % Nothing excluded -> all prohibited
disj_prohib(G, ATIs, ATEs, Vpe) ->
    case in_any(G, ATIs, Vpe) of
	true ->
	    true;
	false ->
	    not(in_any(G, ATEs, Vpe))
    end.

conj_prohib(_G, [], _ATEs, _Vpe) ->
    false; % Nothing included -> nothing prohibited
conj_prohib(G, ATIs, ATEs, Vpe) ->
    case in_all(G, ATIs, Vpe) of
	true ->
	    not(in_any(G, ATEs, Vpe));
	false ->
	    false
    end.

in_any(G, Set, Type) when not is_list(Set) ->
    in_any(G, sets:to_list(Set), Type);
in_any(_G, [], _Vpe) ->
    false;
in_any(G, [AT | Rest], Vpe) ->
    [#pe{vertex = Vat}] = mnesia:dirty_read(pe, AT),
    case digraph:get_path(G, Vpe, Vat) of
	false ->
	    in_any(G, Rest, Vpe);
	_ ->
	    true
    end.

in_all(G, Set, Vpe) when not is_list(Set) ->
    in_all(G, sets:to_list(Set), Vpe);
in_all(_G, [], _Vpe) ->
    false;
in_all(G, [AT | Rest], Vpe) ->
    [#pe{vertex = Vat}] = mnesia:dirty_read(pe, AT),
    case digraph:get_path(G, Vpe, Vat) of
	false ->
	    false;
	_ when Rest =:= [] ->
	    true;
	_ ->
	    in_all(G, Rest, Vpe)
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(EUNIT).

server_test_() ->
    {setup, fun server_setup/0, fun server_cleanup/1, fun tsts/1}.

server_setup() ->
    %% To prevent an existing database being corrupted by running the
    %% tests, a temporary database is created in a separate directory.
    zuuid:start(),
    {ok, Cwd} = file:get_cwd(), % current working directory
    Uuid = zuuid:string(zuuid:v1()), % generate a UUID
    Dir = filename:join([Cwd, "test", Uuid]), % construct temporary directory name
    ok = file:make_dir(Dir), %create dir
    application:set_env(mnesia, dir, Dir), % This is how mnesia will know where the db should go
    pm_db:install([node()]), % install a fresh db
    application:start(mnesia), % start mnesia
    pm_db:start(), % make sure tables are available
    pm_pap:start_link(), % start PAP
    pm_pdp:start_link(), % start PDP
    Dir.

server_cleanup(Dir) ->
    pm_pdp:stop(),
    pm_pap:stop(),
    application:stop(mnesia), % stop mnesia
    %% Delete temporary db by removing db files and the directory
    {ok, Files} = file:list_dir(Dir),
    [file:delete(filename:join([Dir, File])) || File <- Files],
    ok = file:del_dir(Dir),
    ok.

tsts(_Pids) ->
    [tst_policy_machine_core_getting_started(),
     tst_fig3_privilege(), % simple structure
     tst_fig2_privilege(), % more complex structure
     tst_fig4_u_disj_prohibitions(),
     tst_fig4_ua_disj_prohibitions(),
     tst_fig4_u_conj_prohibitions(),
     tst_fig4_1_disj_prohibitions(),
     tst_fig4_2_disj_prohibitions(),
     tst_fig4_3_disj_prohibitions(),
     tst_fig4_4_disj_prohibitions(),
     tst_fig4_5_disj_prohibitions(),
     tst_fig4_6_disj_prohibitions(),
     tst_fig4_7_disj_prohibitions(),
     tst_fig4_8_disj_prohibitions(),
     tst_fig4_1_conj_prohibitions(),
     tst_fig4_2_conj_prohibitions(),
     tst_fig4_3_conj_prohibitions(),
     tst_fig4_4_conj_prohibitions(),
     tst_fig4_5_conj_prohibitions(),
     tst_fig4_6_conj_prohibitions(),
     tst_fig4_7_conj_prohibitions(),
     tst_fig4_8_conj_prohibitions(),
     tst_fig4_9_conj_prohibitions()
    ].

tst_policy_machine_core_getting_started() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "Bank Teller example"}),
    {ok, Branch_1_usr_attr} = pm_pap:c_ua_in_pc(#ua{value = "Branch 1 user attribute"}, PC),
    {ok, Branch_1_obj_attr} = pm_pap:c_oa_in_pc(#oa{value = "Branch 1 object attribute"}, PC),
    {ok, Teller} = pm_pap:c_ua_in_ua(#ua{value = "Teller"}, Branch_1_usr_attr),
    {ok, Auditor} = pm_pap:c_ua_in_ua(#ua{value = "Auditor"}, Branch_1_usr_attr),
    {ok, U1} = pm_pap:c_u_in_ua(#u{value = "User u1"}, Teller),
    {ok, U2} = pm_pap:c_u_in_ua(#u{value = "User u2"}, Auditor),
    {ok, Accounts} = pm_pap:c_oa_in_oa(#oa{value = "accounts"}, Branch_1_obj_attr),
    {ok, O1} = pm_pap:c_o_in_oa(#o{value = "Account o1"}, Accounts),
    AR_r = #ar{id = 'r'},
    AR_w = #ar{id = 'w'},
    %% Branch 1 user attribute has read and write permissions on the Branch 1 object attribute
    pm_pap:c_assoc(Teller, [AR_r, AR_w], Accounts),
    pm_pap:c_assoc(Auditor, [AR_r], Accounts),
    [?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O1))].

tst_fig3_privilege() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{value = "Division"}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{value = "Group1"}, Division),
    {ok, Group2} = pm_pap:c_ua_in_ua(#ua{value = "Group2"}, Division),
    {ok, U1} = pm_pap:c_u_in_ua(#u{value = "u1"}, Group1),
    {ok, U2} = pm_pap:c_u_in_ua(#u{value = "u2"}, Group2),
    {ok, U3} = pm_pap:c_u_in_ua(#u{value = "u3"}, Division),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{value = "Projects"}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{value = "Project1"}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{value = "Project2"}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{value = "o1"}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{value = "o2"}, Project1),
    {ok, O3} = pm_pap:c_o_in_oa(#o{value = "o3"}, Project2),
    AR_r = #ar{id = 'r'},
    AR_w = #ar{id = 'w'},
    pm_pap:c_assoc(Group1, [AR_w], Project1),
    pm_pap:c_assoc(Group2, [AR_w], Project2),
    pm_pap:c_assoc(Division, [AR_r], Projects),
    [?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O3)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U1, [AR_w], O3)),

     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_w], O3)),

     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O3)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O3))].
       
tst_fig2_privilege() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, UA11} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, UA1} = pm_pap:c_ua_in_ua(#ua{}, UA11),
    {ok, U1} = pm_pap:c_u_in_ua(#u{}, UA1),
    {ok, UA12} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, UA2} = pm_pap:c_ua_in_ua(#ua{}, UA12),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, UA2),
    U3 = #u{id = u3}, % Non-existing user
    pm_pap:c_ua_to_ua(UA1, UA12),
    {ok, OA21} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, OA20} = pm_pap:c_oa_in_oa(#oa{}, OA21),
    {ok, OA1} = pm_pap:c_oa_in_oa(#oa{}, OA20),
    {ok, OA2} = pm_pap:c_oa_in_oa(#oa{}, OA20),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, OA1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, OA2),
    O3 = #o{id = o3}, % Non-existing object
    AR_r = #ar{id = 'r'},
    AR_w = #ar{id = 'w'},
    AR_x = #ar{id = 'x'}, % Non-existing access right
    pm_pap:c_assoc(UA12, [AR_r], OA21),
    pm_pap:c_assoc(UA11, [AR_w], OA1),
    [?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U1, [AR_w], O2)),

     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O2)),

     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r, AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U1, [AR_r, AR_w], O2)),

     ?_assertMatch({error, badarg}, pm_pdp:privilege(U1, [], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U1, [AR_x], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U1, [AR_r, AR_x], O1)),
     ?_assertMatch({error, not_found}, pm_pdp:privilege(U3, [AR_r, AR_w], O1)),
     ?_assertMatch({error, not_found}, pm_pdp:privilege(U1, [AR_r, AR_w], O3))].

tst_fig4_u_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, Group2} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U1} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group2),
    {ok, U3} = pm_pap:c_u_in_ua(#u{}, Division),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    AR_w = #ar{id = 'w'},
    pm_pap:c_assoc(Group1, [AR_w], Project1),
    pm_pap:c_assoc(Group2, [AR_w], Project2),
    pm_pap:c_assoc(Division, [AR_r], Projects),
    pm_pap:c_disj_uprohib(U2, [AR_r], [Project1], []),
    [?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O3)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U1, [AR_w], O3)),

     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_w], O3)),

     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O3)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O3))].
       
tst_fig4_ua_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, Group2} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U1} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group2),
    {ok, U3} = pm_pap:c_u_in_ua(#u{}, Division),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    AR_w = #ar{id = 'w'},
    pm_pap:c_assoc(Group1, [AR_w], Project1),
    pm_pap:c_assoc(Group2, [AR_w], Project2),
    pm_pap:c_assoc(Division, [AR_r], Projects),
    pm_pap:c_disj_uaprohib(Group2, [AR_r], [Project1], []),
    [?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O3)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U1, [AR_w], O3)),

     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_w], O3)),

     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O3)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O3))].

tst_fig4_u_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, Group2} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U1} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group2),
    {ok, U3} = pm_pap:c_u_in_ua(#u{}, Division),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    AR_w = #ar{id = 'w'},
    pm_pap:c_assoc(Group1, [AR_w], Project1),
    pm_pap:c_assoc(Group2, [AR_w], Project2),
    pm_pap:c_assoc(Division, [AR_r], Projects),
    pm_pap:c_conj_uprohib(U2, [AR_r], [Project1], []),
    [?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_r], O3)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U1, [AR_w], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U1, [AR_w], O3)),

     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_w], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_w], O3)),

     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U3, [AR_r], O3)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U3, [AR_w], O3))].
       
tst_fig4_1_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_disj_uprohib(U2, [AR_r], [Project1], []),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_2_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_disj_uprohib(U2, [AR_r], [], [Project1]),
    [?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_3_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_disj_uprohib(U2, [AR_r], [Project1], [Projects]),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_4_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_disj_uprohib(U2, [AR_r], [Project2], [Projects]),
    [?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_5_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_disj_uprohib(U2, [AR_r], [Project2], [Project1]),
    [?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_6_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_disj_uprohib(U2, [AR_r], [Project1], [Project2]),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_7_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_disj_uprohib(U2, [AR_r], [Projects], [Project1]),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_8_disj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_disj_uprohib(U2, [AR_r], [Projects], [Project2]),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_1_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [], [Projects]),
    [?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_2_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [Project1], []),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_3_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [Project2], []),
    [?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_4_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [Project1, Project2], []),
    [?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_5_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [Projects], []),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_6_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [Projects], [Project1]),
    [?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_7_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [Projects], [Project2]),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_8_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [Project1], [Project2]),
    [?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O3))].

tst_fig4_9_conj_prohibitions() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, Division} = pm_pap:c_ua_in_pc(#ua{}, PC),
    {ok, Group1} = pm_pap:c_ua_in_ua(#ua{}, Division),
    {ok, U2} = pm_pap:c_u_in_ua(#u{}, Group1),
    {ok, Projects} = pm_pap:c_oa_in_pc(#oa{}, PC),
    {ok, Project1} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, Project2} = pm_pap:c_oa_in_oa(#oa{}, Projects),
    {ok, O1} = pm_pap:c_o_in_oa(#o{}, Project1),
    {ok, O2} = pm_pap:c_o_in_oa(#o{}, Project1),
    ok = pm_pap:c_o_to_oa(O2, Project2),
    {ok, O3} = pm_pap:c_o_in_oa(#o{}, Project2),
    AR_r = #ar{id = 'r'},
    pm_pap:c_assoc(Division, [AR_r], Projects),
    {ok, _} = pm_pap:c_conj_uprohib(U2, [AR_r], [Project2], [Project1]),
    [?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O1)),
     ?_assertMatch(grant, pm_pdp:privilege(U2, [AR_r], O2)),
     ?_assertMatch(deny, pm_pdp:privilege(U2, [AR_r], O3))].

-endif.
