-module(pm_db).

%%% @doc

-include("pm.hrl").

%% API
-export([start/0, install/0, install/1, clear/0]).

%%%===================================================================
%%% API
%%%===================================================================

install() ->
    install([node()]).

install(Nodes) ->
    rpc:multicall(Nodes, application, stop, [mnesia]),
    io:format("Delete old schema, if any~n"),
    mnesia:delete_schema(Nodes),
    io:format("Create new schema~n"),
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    create_tables(),
    init_tables(),
    rpc:multicall(Nodes, application, stop, [mnesia]).

tables() ->
    [pe, u, ua, o, oa, pc, assign, association, 
     u_deny_conj, p_deny_conj, ua_deny_conj,
     u_deny_disj, p_deny_disj, ua_deny_disj, 
     obligation, ar, aop, rop, arset, atiset, ateset,
     pattern, response].

clear() ->
    [mnesia:transaction(mnesia:clear_table(Table)) || Table <- tables()],
    init_tables().
	
start() ->
    ok = mnesia:wait_for_tables(tables(), 5000).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_tables() ->
    [mnesia:dirty_write(u, Record) || Record <- init_u_data()],
    [mnesia:dirty_write(ar, Record) || Record <- init_ar_data()],
    [mnesia:dirty_write(rop, Record) || Record <- init_rop_data()],
    [mnesia:dirty_write(aop, Record) || Record <- init_aop_data()].

create_tables() ->
    %% TODO: The pe table probably can be a RAM only table.
    {atomic, ok} = mnesia:create_table(pe,
                                       [{attributes, record_info(fields, pe)},
                                        {type, set},
                                        {ram_copies, [node()]}
                                       ]),
    {atomic, ok} = mnesia:create_table(u,
                                       [{attributes, record_info(fields, u)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]),
    {atomic, ok} = mnesia:create_table(ua,
                                       [{attributes, record_info(fields, ua)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]),
    {atomic, ok} = mnesia:create_table(o,
                                       [{attributes, record_info(fields, o)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]),
    {atomic, ok} = mnesia:create_table(oa,
                                       [{attributes, record_info(fields, oa)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]),
    {atomic, ok} = mnesia:create_table(pc,
                                       [{attributes, record_info(fields, pc)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]),
    {atomic, ok} = mnesia:create_table(assign,
                                       [{attributes, record_info(fields, assign)},
                                        {type, bag},
					{index, [b]},
                                        {disc_copies, [node()]}
                                       ]),
    {atomic, ok} = mnesia:create_table(association,
                                       [{attributes, record_info(fields, association)},
                                        {type, bag},
					{index, [arset, at]},
                                        {disc_copies, [node()]}
                                       ]),
    [{atomic, ok} = mnesia:create_table(Tbl,
					[{record_name, prohibition},
					 {attributes, record_info(fields, prohibition)},
					 {type, bag},
					 {index, [b, c, d]},
					 {disc_copies, [node()]}
					]) || Tbl <- [u_deny_conj, p_deny_conj, ua_deny_conj,
						      u_deny_disj, p_deny_disj, ua_deny_disj]],
    {atomic, ok} = mnesia:create_table(obligation,
                                       [{attributes, record_info(fields, obligation)},
                                        {type, bag},
					{index, [pattern, response]},
                                        {disc_copies, [node()]}                                   
				       ]),
    [{atomic, ok} = mnesia:create_table(Tbl,
                                       [{record_name, op},
					{attributes, record_info(fields, op)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]) || Tbl <- [aop, rop]],
    {atomic, ok} = mnesia:create_table(ar,
                                       [{attributes, record_info(fields, ar)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]),
    [{atomic, ok} = mnesia:create_table(Tbl,
                                       [{record_name, set},
					{attributes, record_info(fields, set)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]) || Tbl <- [arset, atiset, ateset]],
    [{atomic, ok} = mnesia:create_table(Tbl,
                                       [{record_name, seq},
					{attributes, record_info(fields, seq)},
                                        {type, set},
                                        {disc_copies, [node()]}
                                       ]) || Tbl <- [pattern, response]].
    

init_u_data() ->
    %% Principal Administrator 
    [#u{id = 'PA', value = {"PA", "Principal Administrator"}}].

init_ar_data() ->
    %% Generic Non-administrative Access Rights: See table 1 pg. 57
    [#ar{id = 'r', value = {"r", "Read access right"}},
     #ar{id = 'w', value = {"w", "Write access right"}},

     %% Generic Administrative Access Rights: See table 2 pg. 57
     #ar{id = 'c-u', value = {"c-u", "The right to create a user"}},
     #ar{id = 'd-u', value = {"d-u", "The right to delete a user"}},
     #ar{id = 'c-ua', value = {"c-ua", "The right to create a user attribute"}},
     #ar{id = 'd-ua', value = {"d-ua", "The right to delete a user attribute"}},
     #ar{id = 'c-o', value = {"c-o", "The right to create an object"}},
     #ar{id = 'd-o', value = {"d-o", "The right to delete an object"}},
     #ar{id = 'c-oa', value = {"c-oa", "The right to create an object attribute"}},
     #ar{id = 'd-oa', value = {"d-oa", "The right to delete an object attribute"}},
     #ar{id = 'c-pc', value = {"c-pc", "The right to create a policy class"}},
     #ar{id = 'd-pc', value = {"d-pc", "The right to delete a policy class"}},
     #ar{id = 'c-uua', value = {"c-uua", "The right to create a user in a user attribute"}},
     #ar{id = 'd-uua', value = {"d-uua", "The right to delete a user from a user attribute"}},
     #ar{id = 'c-uaua', value = {"c-uaua", "The right to create a user attribute in a user attribute"}},
     #ar{id = 'd-uaua', value = {"d-uaua", "The right to delete a user attribute from a user attribute"}},
     #ar{id = 'c-uapc', value = {"c-uapc", "The right to create a user attribute in a policy class"}},
     #ar{id = 'd-uapc', value = {"d-uapc", "The right to delete a user attribute from a policy class"}},
     #ar{id = 'c-ooa', value = {"c-ooa", "The right to create an object in an object attribute"}},
     #ar{id = 'd-ooa', value = {"d-ooa", "The right to delete an object from an object attribute"}},
     #ar{id = 'c-oaoa', value = {"c-oaoa", "The right to create an object attribute in an object attribute"}},
     #ar{id = 'd-oaoa', value = {"d-oaoa", "The right to delete an object attribute from an object attribute"}},
     #ar{id = 'c-oapc', value = {"c-oapc", "The right to create an object attribute in a policy class"}},
     #ar{id = 'd-oapc', value = {"d-oapc", "The right to delete an object attribute from a policy class"}},
     #ar{id = 'c-uua-fr', value = {"c-uua-fr", "The right to create the from part of a user to user attribute assignment"}},
     #ar{id = 'd-uua-fr', value = {"d-uua-fr", "The right to delete the from part of a user to user attribute assignment"}},
     #ar{id = 'c-uua-to', value = {"c-uua-to", "The right to create the to part of a user to user attribute assignment"}},
     #ar{id = 'd-uua-to', value = {"d-uua-to", "The right to delete the to part of a user to user attribute assignment"}},
     #ar{id = 'c-uaua-fr', value = {"c-uaua-fr", "The right to create the from part of a user attribute to a user attribute assignment"}},
     #ar{id = 'd-uaua-fr', value = {"d-uaua-fr", "The right to delete the from part of a user attribute to a user attribute assignment"}},
     #ar{id = 'c-uaua-to', value = {"c-uaua-to", "The right to create the to part of a user attribute to a user attribute assignment"}},
     #ar{id = 'd-uaua-to', value = {"d-uaua-to", "The right to delete the to part of a user attribute to a user attribute assignment"}},
     #ar{id = 'c-uapc-fr', value = {"c-uapc-fr", "The right to create the from part of a user attribute to a policy class assignment"}},
     #ar{id = 'd-uapc-fr', value = {"d-uapc-fr", "The right to delete the from part of a user attribute to a policy class assignment"}},
     #ar{id = 'c-uapc-to', value = {"c-uapc-to", "The right to create the to part of a user attribute to a policy class assignment"}},
     #ar{id = 'd-uapc-to', value = {"d-uapc-to", "The right to delete the to part of a user attribute to a policy class assignment"}},
     #ar{id = 'c-ooa-fr', value = {"c-ooa-fr", "The right to create the from part of an object to an object attribute assignment"}},
     #ar{id = 'd-ooa-fr', value = {"d-ooa-fr", "The right to delete the from part of an object to an object attribute assignment"}},
     #ar{id = 'c-ooa-to', value = {"c-ooa-to", "The right to create the to part of an object to an object attribute assignment"}},
     #ar{id = 'd-ooa-to', value = {"d-ooa-to", "The right to delete the to part of an object to an object attribute assignment"}},
     #ar{id = 'c-oaoa-fr', value = {"c-oaoa-fr", "The right to create the from part of an object attribute to an object attribute assignment"}},
     #ar{id = 'd-oaoa-fr', value = {"d-oaoa-fr", "The right to delete the from part of an object attribute to an object attribute assignment"}},
     #ar{id = 'c-oaoa-to', value = {"c-oaoa-to", "The right to create the to part of an object attribute to an object attribute assignment"}},
     #ar{id = 'd-oaoa-to', value = {"d-oaoa-to", "The right to delete the to part of an object attribute to an object attribute assignment"}},
     #ar{id = 'c-oapc-fr', value = {"c-oapc-fr", "The right to create the from part of an object attribute to an policy class assignment"}},
     #ar{id = 'd-oapc-fr', value = {"d-oapc-fr", "The right to delete the from part of an object attribute to an policy class assignment"}},
     #ar{id = 'c-oapc-to', value = {"c-oapc-to", "The right to create the to part of an object attribute to an policy class assignment"}},
     #ar{id = 'd-oapc-to', value = {"d-oapc-to", "The right to delete the to part of an object attribute to an policy class assignment"}},
     #ar{id = 'c-assoc-fr', value = {"c-assoc-fr", "The right to create an association from"}},
     #ar{id = 'd-assoc-fr', value = {"d-assoc-fr", "The right to delete an association from"}},
     #ar{id = 'c-assoc-to', value = {"c-assoc-to", "The right to create an association to"}},
     #ar{id = 'd-assoc-to', value = {"d-assoc-to", "The right to delete an association to"}},
     #ar{id = 'c-prohib-fr', value = {"c-prohib-fr", "The right to create a prohibition from"}},
     #ar{id = 'd-prohib-fr', value = {"d-prohib-fr", "The right to delete a prohibition from"}},
     #ar{id = 'c-prohib-to', value = {"c-prohib-to", "The right to create a prohibition to"}},
     #ar{id = 'd-prohib-to', value = {"d-prohib-to", "The right to delete a prohibition to"}},
     #ar{id = 'c-oblig', value = {"c-oblig", "The right to create an obligation"}},
     #ar{id = 'd-oblig', value = {"d-oblig", "The right to delete an oblication"}},
     #ar{id = 'r-del', value = {"r-del", "The right to delegate read access"}}, % delegate read
     #ar{id = 'w-del', value = {"w-del", "The right to delegate write access"}}]. % delegate write}

init_rop_data() ->
    %% Standard user operations
    [#op{id = 'read', value = {"read", "Read operation"}},
     #op{id = 'write', value = {"write", "Write operation"}}].

init_aop_data() ->
    %% administrative operations: See Appendix D— Administrative Routines pg. 107
    [#op{id = 'c_u_in_ua', value = {"c_u_in_ua", "Create user in user attribute operation"}},
     #op{id = 'd_u_in_ua', value = {"d_u_in_ua", "Delete user from user attribute operation"}},
     #op{id = 'c_ua_in_ua', value = {"c_ua_in_ua", "Create user attribute in user attribute operation"}},
     #op{id = 'd_ua_in_ua', value = {"d_ua_in_ua", "Delete user attribute from user attribute operation"}},
     #op{id = 'c_ua_in_pc', value = {"c_ua_in_pc", "Create user attribute in policy class operation"}},
     #op{id = 'd_ua_in_pc', value = {"d_ua_in_pc", "Delete user attribute from policy class operation"}},
     #op{id = 'c_pc', value = {"c_pc", "Create policy class operation"}},
     #op{id = 'd_pc', value = {"d_pc", "Delete policy class operation"}},
     #op{id = 'c_u_to_ua', value = {"c_u_to_ua", "Create user to user attribute assignment operation"}},
     #op{id = 'd_u_to_ua', value = {"d_u_to_ua", "Delete user to user attribute assignment operation"}},
     #op{id = 'c_ua_to_ua', value = {"c_ua_to_ua", "Create user attribute to user attribute assignment operation"}},
     #op{id = 'd_ua_to_ua', value = {"d_ua_to_ua", "Delete user attribute to user attribute assignment operation"}},
     #op{id = 'c_ua_to_pc', value = {"c_ua_to_pc", "Create user attribute to policy class assignment operation"}},
     #op{id = 'd_ua_to_pc', value = {"d_ua_to_pc", "Delete user attribute to policy class assignment operation"}},
     #op{id = 'c_assoc', value = {"c_assoc", "Create association operation"}},
     #op{id = 'd_assoc', value = {"d_assoc", "Delete association operation"}},
     #op{id = 'c_conj_uprohib', value = {"c_conj_uprohib", "Create conjunctive user prohibition operation"}},
     #op{id = 'd_conj_uprohib', value = {"d_conj_uprohib", "Delete conjunctive user prohibition operation"}},
     #op{id = 'c_conj_pprohib', value = {"c_conj_pprohib", "Create conjunctive process prohibition operation"}},
     #op{id = 'd_conj_pprohib', value = {"d_conj_pprohib", "Delete conjunctive process prohibition operation"}},
     #op{id = 'c_conj_uaprohib', value = {"c_conj_uaprohib", "Create conjunctive user attribute prohibition operation"}},
     #op{id = 'd_conj_uaprohib', value = {"d_conj_uaprohib", "Delete conjunctive user attribute prohibition operation"}},
     #op{id = 'c_disj_uprohib', value = {"c_disj_uprohib", "Create disjunctive user prohibition operation"}},
     #op{id = 'd_disj_uprohib', value = {"d_disj_uprohib", "Delete disjunctive user prohibition operation"}},
     #op{id = 'c_disj_pprohib', value = {"c_disj_pprohib", "Create disjunctive process prohibition operation"}},
     #op{id = 'd_disj_pprohib', value = {"d_disj_pprohib", "Delete disjunctive process prohibition operation"}},
     #op{id = 'c_disj_uaprohib', value = {"c_disj_uaprohib", "Create disjunctive user attribute prohibition operation"}},
     #op{id = 'd_disj_uaprohib', value = {"d_disj_uaprohib", "Delete disjunctive user attribute prohibition operation"}},
     #op{id = 'c_oblig', value = {"c_oblig", "Create obligation operation"}},
     #op{id = 'd_oblig', value = {"d_oblig", "Delete obligation operation"}},
     #op{id = 'eval_pattern', value = {"eval_pattern", "Evaluate pattern operation"}},
     #op{id = 'eval_response', value = {"eval_response", "Evaluate response operation"}}].


