%%% @doc

-module(pm_pip).

-include_lib("eunit/include/eunit.hrl").
-include("pm.hrl").

%% API
-export([create_pc/2, delete_pc/2,
	 create_u_in_ua/3, create_ua_in_ua/3, create_ua_in_pc/3,
	 create_o_in_oa/3, create_oa_in_oa/3, create_oa_in_pc/3,
	 delete_u/2, delete_ua/2, delete_o/2, delete_oa/2,
	 create_rop/1, create_aop/1, create_ar/1, create_arset/2,
	 delete_rop/1, delete_aop/1, delete_ar/1, delete_arset/1,
	 create_atiset/2, create_ateset/2, create_pattern/2, create_response/2,
	 delete_atiset/1, delete_ateset/1, delete_pattern/1, delete_response/1,
	 allocate_id/1, allocate_id/2,
	 create_p/3, delete_p/2, 
	 create_pumapping/3, create_assign/3, create_assoc/3, create_prohib/5, create_oblig/3,
	 delete_pumapping/3, delete_assign/3, delete_assoc/3, delete_prohib/5, delete_oblig/3,
	 process_user/2,
	 transaction/1
	]).

-export([users/2, objects/2, elements/2, icap/2, iae/2]).

%% TODO: move the following functions to the module pm_pdp?
-export([disj_range/3, conj_range/3]).

-export([new/0, delete/1, rebuild/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> digraph:graph().
%% @doc Creates and returns a new digraph.
new() ->
    digraph:new([acyclic, protected]).

-spec delete(G :: digraph:graph()) -> ok.
%% @doc Deletes digraph `G'. 
delete(G) ->
    digraph:delete(G),
    ok.

%% C.2 Element Creation Commands
-spec create_u_in_ua(G, U, UA) -> {ok, pm:u()} | no_return() when
      G :: digraph:graph(),
      U :: pm:u(),
      UA :: pm:ua().
%% @doc add user `U' to the policy representation and assign it to user attribute `UA'
create_u_in_ua(G, #u{id = X} = U, #ua{id = Y}) ->
    create_x_in_y(G, X, Y),
    mnesia:write(U),
    {ok, U}.

-spec create_ua_in_ua(G, UA1, UA2) -> {ok, pm:ua()} | no_return() when
      G :: digraph:graph(),
      UA1 :: pm:ua(),
      UA2 :: pm:ua().
%% @doc add user attribute `UA1' and assign it to user attribute `UA2'
create_ua_in_ua(G, #ua{id = X} = UA1, #ua{id = Y}) ->
    create_x_in_y(G, X, Y),
    mnesia:write(UA1),
    {ok, UA1}.

-spec create_ua_in_pc(G, UA, PC) -> {ok, pm:ua()} | no_return() when
      G :: digraph:graph(),
      UA :: pm:ua(),
      PC :: pm:pc().
%% @doc add user attribute `UA' and assign it to policy class `PC'
create_ua_in_pc(G, #ua{id = X} = UA, #pc{id = Y}) ->
    create_x_in_y(G, X, Y),
    mnesia:write(UA),
    {ok, UA}.

-spec create_o_in_oa(G, O, OA) -> {ok, pm:o()} | no_return() when
      G :: digraph:graph(),
      O :: pm:o(),
      OA :: pm:oa().
%% @doc add object `O' to the policy representation and assign it to object attribute `OA'
create_o_in_oa(G, #o{id = X} = O, #oa{id = Y}) ->
    create_x_in_y(G, X, Y),
    mnesia:write(O),
    {ok, O}.

-spec create_oa_in_oa(G, OA1, OA2) -> {ok, pm:oa()} | no_return() when
      G :: digraph:graph(),
      OA1 :: pm:oa(),
      OA2 :: pm:oa().
%% @doc add object attribute `OA1' and assign it to object attribute `OA2'
create_oa_in_oa(G, #oa{id = X} = OA1, #oa{id = Y}) ->
    create_x_in_y(G, X, Y),
    mnesia:write(OA1),
    {ok, OA1}.

-spec create_oa_in_pc(G, OA, PC) -> {ok, pm:oa()} | no_return() when
      G :: digraph:graph(),
      OA :: pm:oa(),
      PC :: pm:pc().
%% @doc add object attribute `OA' and assign it to policy class `PC'
create_oa_in_pc(G, #oa{id = X} = OA, #pc{id = Y}) ->
    create_x_in_y(G, X, Y),
    mnesia:write(OA),
    {ok, OA}.

-spec create_x_in_y(G :: digraph:graph(), X :: pm:id(), Y :: pm:id()) -> ok | no_return().
%% @doc add `X' and assign it to `Y' in digraph `G'. Also add `X' to
%% the set PE and {X, Y} to the set ASSIGN.
create_x_in_y(G, X, Y) ->
    [PEY] = mnesia:read(pe, Y, write),
    case digraph:add_vertex(G, X) of
	{error, Reason} ->
	    erlang:error(Reason);
	_X ->
	    %% TODO: is label used? should the label be {X, Y} as an ASSIGN?
	    case digraph:add_edge(G, X, Y) of
		{error, Reason} ->
		    erlang:error(Reason);
		Edge ->
		    %% ref_cnt for X is 1 because of the assignment
		    mnesia:write(#pe{id = X, ref_cnt = 1}),
		    mnesia:write(PEY#pe{ref_cnt = PEY#pe.ref_cnt + 1}),
		    mnesia:write(#assign{a = X, b = Y, edge = Edge})
	    end
    end.

-spec create_pc(G, PC) -> {ok, pm:pc()} | no_return() when
      G :: digraph:graph(),
      PC :: pm:pc().
%% @doc add a policy class `PC' to the policy representation
create_pc(G, #pc{id = X} = PC) ->
    create_x(G, X),
    mnesia:write(PC),
    {ok, PC}.
    
-spec create_x(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
%% @doc add a policy element x to the digraph `G' and to the set PE.
create_x(G, X) ->
    case digraph:add_vertex(G, X) of
	{error, Reason} ->
	    erlang:error(Reason);
	_X ->
	    mnesia:write(#pe{id = X, ref_cnt = 0})
    end.

%% C.3 Element Deletion Commands
-spec delete_u(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
delete_u(G, X) ->
    [#u{}] = mnesia:read(u, X, write),
    delete_x(G, X),
    mnesia:delete({u, X}).

-spec delete_ua(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
delete_ua(G, X) ->
    [#ua{}] = mnesia:read(ua, X, write),
    delete_x(G, X),
    mnesia:delete({ua, X}).

-spec delete_o(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
delete_o(G, X) ->
    [#o{}] = mnesia:read(o, X, write),
    delete_x(G, X),
    mnesia:delete({o, X}).

-spec delete_oa(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
delete_oa(G, X) ->
    [#oa{}] = mnesia:read(oa, X, write),
    delete_x(G, X),
    mnesia:delete({oa, X}).

-spec delete_pc(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
delete_pc(G, X) ->
    [#pc{}] = mnesia:read(pc, X, write),
    delete_x(G, X),
    mnesia:delete({pc, X}).

-spec delete_x(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
%% @doc delete a policy element x from the policy representation
delete_x(G, X) ->
    %% ref_cnt must be zero for this action!  
    [#pe{ref_cnt = 0} = PE] = mnesia:read(pe, X, write),
    digraph:del_vertex(G, X),
    mnesia:delete_object(PE).
    
%% C.4 Entity creation
-spec create_p(P :: pm:p(), U :: pm:u(), PU :: [#process_user{}]) -> [#process_user{}].
%% @doc add process x and map it to user y
create_p(P, U, PU) ->
    link(P),
    [#process_user{p = P, u = U} | PU].

-spec create_rop(Op :: pm:op()) -> ok.
%% @doc add a resource operation to the policy representation
create_rop(Op) ->
    create_op(Op, rop).

-spec create_aop(Op :: pm:op()) -> ok.
%% @doc add an administrative operation to the policy representation
create_aop(Op) ->
    create_op(Op, aop).

create_op(Op, Op_table) ->
    mnesia:write(Op_table, Op#op{ref_cnt = 0}, write).

-spec create_ar(AR :: pm:ar()) -> ok.
%% @doc add an access right to the policy representation
create_ar(AR) ->
    mnesia:write(AR#ar{ref_cnt = 0}).

-spec create_arset(ARset :: pm:id(), AR_ids :: nonempty_list(pm:id())) -> ok | no_return().
%% The calling function does take care the AR_ids are sorted without duplicates. 
create_arset(ARset, AR_ids) ->
    case mnesia:read(arset, ARset, read) of
	[] ->
	    Value = sets:from_list(AR_ids),
	    [begin 
		 [AR] = mnesia:read(ar, AR_id, write),
		 mnesia:write(AR#ar{ref_cnt = AR#ar.ref_cnt + 1})
	     end || AR_id <- sets:to_list(Value)],
	    Set = #set{id = ARset, value = Value, ref_cnt = 0, inst_cnt = 1},
	    mnesia:write(arset, Set, write);
	[#set{inst_cnt = N} = Set] ->
	    mnesia:write(arset, Set#set{inst_cnt = N + 1}, write)
    end.
    
-spec create_atiset(ATIset :: pm:id(), ATIs :: list(pm:at())) -> ok | no_return().
%% @doc add a set of inclusion policy elements denoted by a referent
%% attribute to the representation
create_atiset(ATIset, ATI_ids) ->
    create_atset(ATIset, ATI_ids, atiset).

-spec create_ateset(ATEset :: pm:id(), ATE_ids :: list(pm:at())) -> ok | no_return().
%% @doc add a set of exclusion policy elements denoted by a referent
%% attribute to the representation
create_ateset(ATEset, ATE_ids) ->
    create_atset(ATEset, ATE_ids, ateset).

%% TODO: merge create_arset and create_atset into a single
%% function. These are really similar.
create_atset(ATset, AT_ids, ATset_table) ->
    case mnesia:read(ATset_table, ATset, read) of
	[] ->
	    Value = sets:from_list(AT_ids),
	    [begin 
		 [PE] = mnesia:read(pe, AT_id, write),
		 mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt + 1})
	     end || AT_id <- sets:to_list(Value)],
	    Set = #set{id = ATset, value = Value, ref_cnt = 0, inst_cnt = 1},
	    mnesia:write(ATset_table, Set, write);
	[#set{inst_cnt = N} = Set] ->
	    mnesia:write(ATset_table, Set#set{inst_cnt = N + 1}, write)
    end.
    
-spec create_pattern(X :: pm:id(), Y :: nonempty_list(pm:pattern())) -> ok.
%% @doc add an event pattern to the policy representation
create_pattern(X, Y) ->
    create_seq(X, Y, pattern).

-spec create_response(X :: pm:id(), Y :: nonempty_list(pm:response())) -> ok.
%% @doc add an event response to the policy representation
create_response(X, Y) ->
    create_seq(X, Y, response).

create_seq(X, Y, Table) ->
    Seq = #seq{id = X, value = Y, ref_cnt = 0},  
    mnesia:write(Table, Seq, write).
  
-spec allocate_id() -> pm:id().
%% @doc allocate an identifier not yet in use by the PM framework
allocate_id() ->
    {uuid, Id} =  zuuid:v4(),
    Id.

-spec allocate_id(Tag :: pm:id_tag()) -> pm:id().
%% @doc allocate a tagged identifier not yet in use by the PM framework
allocate_id(Tag) ->
    Id = allocate_id(),
    {Tag, Id}.

%% @doc allocate an identifier for use in the PM framework based on a
%% list of values. This is used for determining the value of a set
%% id. Instead of generating new ids for the same piece of data, data
%% is reused and reference counted.
%%
%% TODO: we use a 32 bit hash (maybe 27 bits) to calculate a key based
%% on the value(s) passed to the allocate_id function. Is the range
%% sufficient? Use hash functions from the crypto lib?
-spec allocate_id(Tag :: pm:id_tag(), Values :: nonempty_list(term())) -> pm:id().
allocate_id(Tag, Values) when is_list(Values) ->
    %% TODO: can range be set to its default of 0 .. 2^27 - 1? Once
    %% set, you can _NOT_ change the range anymore since the result of
    %% the hash differs!
    %% erlang:phash2(Values). 
    %%
    %% NB: the hash is taken from the elements in the list, not the
    %% list itself, i.e. even if the implementation of the hash
    %% changes, the hash function will still return the same id.
    {Tag, erlang:phash2(lists:usort(Values), 16#100000000)}.

%% C.5 Entity deletion
-spec delete_p(X :: pm:p(), PU :: [#process_user{}]) -> [#process_user{}] | no_return().
%% @doc remove process x from the policy representation
delete_p(P, PU1) ->
    {value, #process_user{}, PU2} = lists:keytake(P, #process_user.p, PU1),
    unlink(P),
    PU2.

-spec delete_rop(X :: pm:id()) -> ok | no_return().
%% @doc remove a resource operation from the policy representation
delete_rop(X) ->
    delete_op(X, rop).

-spec delete_aop(X :: pm:id()) -> ok | no_return().
%% @doc remove an administrative operation from the policy representation
delete_aop(X) ->
    delete_op(X, aop).

delete_op(X , Op_table) ->
    [#op{ref_cnt = 0}] = mnesia:read(Op_table, X, write),
    mnesia:delete({Op_table, X}).
    
-spec delete_ar(X :: pm:id()) -> ok | no_return().
%% @doc remove an access right from the policy representation
delete_ar(X) ->
    [#ar{ref_cnt = 0}] = mnesia:read(ar, X, write),
    mnesia:delete({ar, X}).

-spec delete_arset(X :: pm:id()) -> ok | no_return().
%% @doc remove an access right set from the policy representation
delete_arset(X) ->
    case mnesia:read(arset, X, write) of
    	[#set{value = ARset, inst_cnt = 1, ref_cnt = 0}] ->
    	    [begin 
    		 [AR] = mnesia:read(ar, AR_id, write),
    		 mnesia:write(AR#ar{ref_cnt = AR#ar.ref_cnt - 1})
    	     end || AR_id <- sets:to_list(ARset)],
    	    mnesia:delete({arset, X});
    	[#set{inst_cnt = N, ref_cnt = M} = Set] when N > M ->
	    mnesia:write(arset, Set#set{inst_cnt = N - 1}, write)
    end.

-spec delete_atiset(X :: pm:id()) -> ok | no_return().
%% @doc remove a set of inclusion attributes from the representation
delete_atiset(X) ->
    delete_atset(X, atiset).

-spec delete_ateset(X :: pm:id()) -> ok | no_return().
%% @doc remove a set of exclusion attributes from the representation
delete_ateset(X) ->
    delete_atset(X, ateset).

%% TODO: merge delete_atset and delete_arset into a single
%% function. They are very similar.
delete_atset(X, ATset_table) ->
    case mnesia:read(ATset_table, X, write) of
    	[#set{value = ATset, inst_cnt = 1, ref_cnt = 0}] ->
    	    [begin 
    		 [PE] = mnesia:read(pe, AT_id, write),
    		 mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt - 1})
    	     end || AT_id <- sets:to_list(ATset)],
    	    mnesia:delete({ATset_table, X});
    	[#set{inst_cnt = N, ref_cnt = M} = Set] when N > M ->
	    mnesia:write(ATset_table, Set#set{inst_cnt = N - 1}, write)
    end.

-spec delete_pattern(X :: pm:id()) -> ok | no_return().
%% @doc delete an event pattern from the policy representation
delete_pattern(X) ->
    delete_seq(X, pattern).

-spec delete_response(X :: pm:id()) -> ok | no_return().
%% @doc delete an event response from the policy representation
delete_response(X) ->
    delete_seq(X, response).

delete_seq(X, Table) ->
    [#seq{ref_cnt = 0}] = mnesia:read(Table, X, write),
    mnesia:delete({Table, X}).

%% C.6 Relation Formation Commands
-spec create_pumapping(P :: pm:p(), U :: pm:u(), PU :: [#process_user{}]) -> [#process_user{}] | no_return().
%% @doc add a maplet pair to the function
create_pumapping(P, U, PU1) ->
    {value, #process_user{u = undefined}, PU2} = lists:keytake(P, #process_user.p, PU1),
    [#process_user{p = P, u = U} | PU2].

-spec create_assign(G :: digraph:graph(), X :: pm:id(), Y :: pm:id()) -> ok | no_return().
%% @doc add tuple (x, y) to the assignment relation
create_assign(G, X, Y) ->
    [#pe{} = PEX] = mnesia:read(pe, X, write),
    [#pe{} = PEY] = mnesia:read(pe, Y, write),
    case digraph:add_edge(G, X, Y) of
	{error, Reason} ->
	    erlang:error(Reason);
	Edge ->
	    mnesia:write(PEX#pe{ref_cnt = PEX#pe.ref_cnt + 1}),
	    mnesia:write(PEY#pe{ref_cnt = PEY#pe.ref_cnt + 1}),
	    mnesia:write(#assign{a = X, b = Y, edge = Edge})
    end.

-spec create_assoc(X :: pm:id(), Y :: pm:id(), Z :: pm:id()) -> ok | no_return().
%% @doc add tuple (x, y, z) to the association relation
create_assoc(X, Y, Z) ->
    [PEX] = mnesia:read(pe, X, write),
    [ARset] = mnesia:read(arset, Y, write),
    [PEZ] = mnesia:read(pe, Z, write),
    mnesia:write(PEX#pe{ref_cnt = PEX#pe.ref_cnt + 1}),
    mnesia:write(arset, ARset#set{ref_cnt = ARset#set.ref_cnt + 1}, write),
    mnesia:write(PEZ#pe{ref_cnt = PEZ#pe.ref_cnt + 1}),
    mnesia:write(#association{ua = X, arset = Y, at = Z}).

-spec create_prohib(W, X, Y, Z, Prohib_table) -> ok | no_return() when
      W :: pm:id(),
      X :: pm:id(),
      Y :: pm:id(),
      Z :: pm:id(),
      Prohib_table :: u_deny_conj | p_deny_conj | ua_deny_conj | 
		      u_deny_disj | p_deny_disj | ua_deny_disj.
%% @doc add tuple (w, x, y, z) to the prohibition relation
create_prohib(W, X, Y, Z, Prohib_table) ->
    [PE] = mnesia:read(pe, W, write),
    [ARset] = mnesia:read(arset, X, write),
    [ATIset] = mnesia:read(atiset, Y, write),
    [ATEset] = mnesia:read(ateset, Z, write),
    mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt + 1}),
    mnesia:write(arset, ARset#set{ref_cnt = ARset#set.ref_cnt + 1}, write),
    mnesia:write(atiset, ATIset#set{ref_cnt = ATIset#set.ref_cnt + 1}, write),
    mnesia:write(ateset, ATEset#set{ref_cnt = ATEset#set.ref_cnt + 1}, write),
    mnesia:write(Prohib_table, #prohibition{a = W, b = X, c = Y, d = Z}, write).

-spec create_oblig(X :: pm:id(), Y :: pm:id(), Z :: pm:id()) -> ok | no_return().
%% @doc add tuple (x, y, z) to the obligation relation
create_oblig(X, Y, Z) ->
    [PE] = mnesia:read(pe, X, write),
    [Pattern] = mnesia:read(pattern, Y, write),
    [Response] = mnesia:read(response, Z, write),
    mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt + 1}),
    mnesia:write(pattern, Pattern#seq{ref_cnt = Pattern#seq.ref_cnt + 1}, write),
    mnesia:write(response, Response#seq{ref_cnt = Response#seq.ref_cnt + 1}, write),
    mnesia:write(#obligation{u = X, pattern = Y, response = Z}).

%% C.7 Relation Rescindment Commands
-spec delete_pumapping(P :: pm:p(), U :: pm:id(), PU :: [#process_user{}]) -> [#process_user{}] | no_return().
%% @doc delete a maplet pair from the function
delete_pumapping(P, U, PU1) ->
    {value, #process_user{u = U}, PU2} = lists:keytake(P, #process_user.p, PU1),
    [#process_user{p = P, u = undefined} | PU2].
    
-spec delete_assign(G :: digraph:graph(), X :: pm:id(), Y :: pm:id()) -> ok | no_return().
%% @doc remove tuple (x, y) from the assignment relation
delete_assign(G, X, Y) ->
    [PEX] = mnesia:read(pe, X, write),
    [PEY] = mnesia:read(pe, Y, write),
    [#assign{edge = Edge} = Row] = mnesia:match_object(#assign{a = X, b = Y, _ = '_'}),
    digraph:del_edge(G, Edge),
    mnesia:write(PEX#pe{ref_cnt = PEX#pe.ref_cnt - 1}),
    mnesia:write(PEY#pe{ref_cnt = PEY#pe.ref_cnt - 1}),
    mnesia:delete_object(Row).

-spec delete_assoc(X :: pm:id(), Y :: pm:id(), Z :: pm:id()) -> ok | no_return().
%% @doc remove tuple (x, y, z) from the association relation
delete_assoc(X, Y, Z) ->
    [PEX] = mnesia:read(pe, X, write),
    [ARset] = mnesia:read(arset, Y, write),
    [PEZ] = mnesia:read(pe, Z, write),
    [Row] = mnesia:match_object(#association{ua = X, arset = Y, at = Z, _ = '_'}),
    mnesia:write(PEX#pe{ref_cnt = PEX#pe.ref_cnt - 1}),
    mnesia:write(arset, ARset#set{ref_cnt = ARset#set.ref_cnt - 1}, write),
    mnesia:write(PEZ#pe{ref_cnt = PEZ#pe.ref_cnt - 1}),
    mnesia:delete_object(Row).

-spec delete_prohib(W, X, Y, Z, Prohib_table) -> ok | no_return() when
      W :: pm:pid(),
      X :: pm:pid(),
      Y :: pm:pid(),
      Z :: pm:pid(),
      Prohib_table :: u_deny_conj | p_deny_conj | ua_deny_conj | 
		      u_deny_disj | p_deny_disj | ua_deny_disj.
%% @doc delete prohibition
delete_prohib(W, X, Y, Z, Prohib_table) ->
    [PE] = mnesia:read(pe, W, write),
    [ARset] = mnesia:read(arset, X, write),
    [ATIset] = mnesia:read(atiset, Y, write),
    [ATEset] = mnesia:read(ateset, Z, write),
    mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt - 1}),
    mnesia:write(arset, ARset#set{ref_cnt = ARset#set.ref_cnt - 1}, write),
    mnesia:write(atiset, ATIset#set{ref_cnt = ATIset#set.ref_cnt - 1}, write),
    mnesia:write(ateset, ATEset#set{ref_cnt = ATEset#set.ref_cnt - 1}, write),
    [Row] = mnesia:match_object(Prohib_table, #prohibition{a = W, b = X, c = Y, d = Z, _ = '_'}, write),
    mnesia:delete_object(Prohib_table, Row, write).

-spec delete_oblig(X :: pm:id(), Y :: pm:id(), Z :: pm:id()) -> ok | no_return().
%% @doc remove tuple (x, y, z) from the obligation relation
delete_oblig(X, Y, Z) ->
    [PE] = mnesia:read(pe, X, write),
    [Pattern] = mnesia:read(pattern, Y, write),
    [Response] = mnesia:read(response, Z, write),
    mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt - 1}),
    mnesia:write(pattern, Pattern#seq{ref_cnt = Pattern#seq.ref_cnt - 1}, write),
    mnesia:write(response, Response#seq{ref_cnt = Response#seq.ref_cnt - 1}, write),
    [Row] = mnesia:match_object(#obligation{u = X, pattern = Y, response = Z, _ = '_'}),
    mnesia:delete_object(Row).

-spec process_user(P :: pm:p(), PU :: [#process_user{}]) -> pm:id() | false.
%% @doc returns the user u ∈ U associated with process p ∈ P.
process_user(P, PU) ->
    case lists:keyfind(P, #process_user.p, PU) of
	#process_user{u = U} ->
	    U;
	_ ->
	    false
    end.

-spec transaction(fun()) -> Result :: term() | {error, Reason :: term()}.
%% @doc Executes the functional object `Fun' with arguments Args as a
%% transaction. This function is used mainly by the PAP to execute a
%% number of PIP functions as a single transaction.
%% @see mneisa:transaction/2
transaction(Fun) ->
    case mnesia:transaction(Fun) of
	{atomic, Result} ->
	    Result;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%% TODO: review use of transactions. Should we go 'dirty' because this
%% function is called by a single pm_pip only or will we use more than
%% one pm_pip and is Mnesia distributed?
rebuild(G) ->
    mnesia:clear_table(pe),
    F = fun() ->
    		Ids = mnesia:select(pc, [{#pc{id = '$1', _='_'}, [], ['$1']}]),
    		[begin
    		     create_x(G, Id),
    		     rebuild_children(G, Id)
    		 end || Id <- Ids]
    	end,
    transaction(F).

%% @doc Given an element `P', fetch the children of `P' from the
%% database by looking up it's assignments and add them to the
%% digraph.
rebuild_children(G, P) ->
    Assigns = mnesia:select(assign, [{#assign{b = '$2', _ = '_'}, [{'=:=', '$2', {P}}], ['$_']}]), 
    [rebuild_child(G, Assign) || Assign <- Assigns].

%% TODO: merge the following code with the create_xxx code
%% above. Manipulating PE should be in one single function if possible
%% to make code maintenance easier.
rebuild_child(G, #assign{a = X, b = Y} = Assign) ->
    case digraph:add_vertex(G, X) of
	{error, Reason} ->
	    erlang:error(Reason);
	_X ->
	    case mnesia:read(pe, X) of
		[] ->
		    mnesia:write(#pe{id = X, ref_cnt = 1});
		[PE]->
		    mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt + 1})
	    end,
	    case digraph:add_edge(G, X, Y) of
	    	{error, Reason} ->
	    	    erlang:error(Reason);
	    	Edge ->
	    	    mnesia:delete_object(Assign),
	    	    mnesia:write(Assign#assign{edge = Edge})
	    end,
	    rebuild_children(G, X)
    end.

-spec users(G, UA) -> [U] when
      G :: digraph:graph(),
      UA :: pm:ua() | pm:id(),
      U :: pm:id().
%% @doc The contained users mapping `users' function is a total
%% function from UA to 2^U, which represents the mapping from a user
%% attribute to the set of users that are contained by that user
%% attribute. Intuitively, the function `users(UA)' returns the set of
%% users that are contained by or possess the characteristics of the
%% user attribute UA.
users(G, #ua{id = X}) -> 
    users(G, X);
users(G, X) -> 
    [U || {u, _} = U <- digraph_utils:reaching_neighbours([X], G)].

-spec objects(G, OA) -> [O] when
      G :: digraph:graph(),
      OA :: pm:oa() | pm:id(),
      O :: pm:id().
%% @doc the reflexive and contained objects mapping `objects' function
%% represents the mapping from an object attribute to the set of
%% objects that are contained by that object attribute. Intuitively,
%% the function `objects(OA)' returns the set of objects that are
%% contained by or possess the characteristics of the object attribute
%% OA.  Since all objects are, by definition, members of the set of
%% object attributes, the function must also include the domain of the
%% function within its range, in such instances.
objects(G, #o{id = X}) -> 
    objects(G, X);
objects(G, #oa{id = X}) -> 
    objects(G, X);
objects(G, X) -> 
    case digraph:vertex(G, X) of
	{_X, _Label} ->
	    [O || {o, _} = O <- digraph_utils:reaching([X], G)];
	false ->
	    []
    end.

-spec elements(G, PE) -> [E] when
      G :: digraph:graph(),
      PE :: pm:pe() | pm:id(),
      E :: pm:pe().
%% @doc Reflexive and Contained Elements Mapping: the `elements'
%% function represents the mapping from a given policy element `PE' to
%% the set of policy elements that includes the policy element and all
%% the policy elements contained by that policy element.
elements(G, PE) ->
    case PE of
	#pc{id = X} -> X;
	#ua{id = X} -> X;
	#u{id = X} -> X;
	#oa{id = X} -> X;
	#o{id = X} -> X
    end,
    case digraph:vertex(G, X) of
	{_X, _Label} ->
	    digraph_utils:reaching([X], G);
	false ->
	    []
    end.

-spec icap(G, UA) -> [{ARset, AT}] when
      G :: digraph:graph(),
      UA :: pm:ua(),
      ARset :: pm:id(),
      AT :: pm:ua() | pm:oa() | pm:o().
%% @doc The `icap' function returns the Inherent Capabilities of a
%% User Attribute `UA'.
icap(G, #ua{id = X}) ->
    %% TODO: maybe use a mnesia (dirty) select instead of a read and
    %% return the two fields of interest directly
    UAs = [UA || {ua, _} = UA <- digraph_utils:reaching([X], G)],
    F = fun(UA, Acc) ->
		mnesia:read(association, UA) ++ Acc
	end,
    Assocs = lists:foldl(F, [], UAs),
    [{ARset, AT} || #association{arset = ARset, at = AT} <- Assocs].

-spec iae(G, AT) -> [{UA, ARset}] when
      G :: digraph:graph(),
      UA :: pm:ua(),
      ARset :: pm:id(),
      AT :: pm:ua() | pm:oa() | pm:o().
%% @doc The `iae' function returns the Inherent Aeabilities of a
%% User Attribute `UA'.
iae(G, #ua{id = X}) ->
    iae(G, X);
iae(G, #o{id = X}) ->
    iae(G, X);
iae(G, #oa{id = X}) ->
    iae(G, X);
iae(G, X) ->
    %% TODO: maybe use a mnesia (dirty) select instead of a indexed
    %% read and return the two fields of interest directly
    ATs = [AT || {Tag, _} = AT <- digraph_utils:reachable([X], G), 
		 Tag =:= oa orelse Tag =:= o orelse Tag =:= ua],
    F = fun(AT, Acc) ->
		mnesia:index_read(association, AT, #association.at) ++ Acc
	end,
    Assocs = lists:foldl(F, [], ATs),
    [{UA, ARset} || #association{ua = UA, arset = ARset} <- Assocs].

%% @see pm_pap:disj_range/2.

%% @doc disjunctive range function implementation following the specs
%% in par. 3.4 p25,
%%
%% TODO: ATIs and ATEs now can contain any kind of PE, not only
%% ATs. It is up to the calling functions to pass in the correct
%% data. For example, the function accepts PCs and Us as valid ATs,
%% which is not correct.
%%
%% This function and also the conj_range function are probably of only
%% little use because they use all PE minus PC for calculating the
%% 'excluded' attributes. In a real live system, PE may be (very)
%% large making the function very expensive.
disj_range(G, ATIs, ATEs) ->
    Set1 = sets:new(),
    F1 = fun(AT, Acc) ->
		 ATs = elements(G, AT),
		 sets:union(Acc, sets:from_list(ATs))
	 end,
    T1 = lists:foldl(F1, Set1, ATIs),
    PEPC = pepc(G),
    Set2 = sets:new(),
    F2 = fun(AT, Acc) ->
    		 ATs = elements(G, AT),
    		 sets:union(Acc, sets:subtract(PEPC, sets:from_list(ATs)))
    	 end,
    T2 = lists:foldl(F2, Set2, ATEs),
    sets:to_list(sets:union(T1, T2)).

%% @doc conjunctive range function implementation following the specs
%% in par. 3.4 p25,
%%
%% @see pm_pap:conj_range/2 and @see disj_range/2.
conj_range(_G, [], _ATEs) ->
    [];
conj_range(G, [ATI | ATIs], ATEs) ->
    Set1 = sets:from_list(elements(G, ATI)),
    F1 = fun(AT, Acc) ->
		 ATs = elements(G, AT),
		 sets:intersection(Acc, sets:from_list(ATs))
	 end,
    T1 = lists:foldl(F1, Set1, ATIs),
    PEPC = pepc(G),
    Set2 = PEPC,
    F2 = fun(AT, Acc) ->
		 ATs = elements(G, AT),
		 sets:intersection(Acc, sets:subtract(PEPC, sets:from_list(ATs)))
	 end,
    T2 = lists:foldl(F2, Set2, ATEs),
    sets:to_list(sets:intersection(T1, T2)).

pepc(G) ->
    PEPC = [PE || {Tag, _Id} = PE <- digraph:vertices(G), Tag =/= pc],
    sets:from_list(PEPC).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(EUNIT).

%% Tests are wrapped in a Mnesia transaction and all are aborted to
%% revert all changes in the database. This should prevent polluting
%% an existing database and / or prevent tests to interfere.

t(Fun) ->
    T = fun() ->
		Rv = Fun(),
		mnesia:abort({ok, Rv})
	end,
    case mnesia:transaction(T) of
	{aborted, {ok, Result}} ->
	    Result;
	Error ->
	    Error
    end.

pm_pip_test_() ->
    {setup, local,
     fun setup/0,
     fun cleanup/1,
     [tst_create_x(),
      tst_delete_x(),
      tst_create_x_in_y(),
      tst_delete_x_in_y(),
      tst_assign(),
      tst_create_rop(),
      tst_delete_rop(),
      tst_create_aop(),
      tst_delete_aop(),
      tst_create_ar(),
      tst_delete_ar(),
      tst_create_arset(),
      tst_delete_arset(),
      tst_create_atset(),
      tst_create_atiset(),
      tst_create_ateset(),
      tst_delete_atset(),
      tst_delete_atiset(),
      tst_delete_ateset(),
      tst_create_seq(),
      tst_create_pattern(),
      tst_create_response(),
      tst_delete_pattern(),
      tst_delete_response(),
      tst_allocate_id(),
      tst_create_assoc(),
      tst_delete_assoc(),
      tst_create_prohib(),
      tst_create_oblig(),
      tst_delete_oblig(),
      tst_create_p(),
      tst_delete_p(),
      tst_pumapping()
     ]}.

setup() ->
    zuuid:start(),
    {ok, Cwd} = file:get_cwd(), % current working directory
    Uuid = zuuid:string(zuuid:v1()), % generate a UUID
    Dir = filename:join([Cwd, "test", Uuid]), % construct temporary directory name
    ok = file:make_dir(Dir), %create dir
    application:set_env(mnesia, dir, Dir), % This is how mnesia will know where the db should go
    pm_db:install([node()]), % install a fresh db
    application:start(mnesia), % start mnesia
    pm_db:start(), % make sure tables are available
    Dir.

cleanup(Dir) ->
    application:stop(mnesia), % stop mnesia
    %% Delete temporary db by removing db files and the directory
    {ok, Files} = file:list_dir(Dir),
    [file:delete(filename:join([Dir, File])) || File <- Files],
    ok = file:del_dir(Dir),
    ok.


tst_create_x() ->
    G = digraph:new([acyclic]),
    F = fun() -> create_x(G, a) end,
    [?_assertEqual(ok, t(F))].

tst_delete_x() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 create_x(G, a),
		 delete_x(G, a)
	 end,
    F2 = fun() -> % fail
		 delete_x(G, a)
	 end,
    F3 = fun() -> % fail
		 create_x(G, b),
		 delete_x(G, c)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2)),
     ?_assertNotEqual(ok, t(F3))].

tst_create_x_in_y() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 create_x(G, mama),
		 create_x_in_y(G, child, mama)
	 end,
    F2 = fun() -> % fail
		 create_x_in_y(G, child, papa)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2))].

tst_delete_x_in_y() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 create_x(G, parent),
		 create_x_in_y(G, child, parent),
		 delete_assign(G, child, parent),
		 delete_x(G, child),
		 delete_x(G, parent)
	 end,
    F2 = fun() -> % fail
		 create_x(G, parent),
		 create_x_in_y(G, child, parent),
		 delete_x(G, child) % fail since child has assignment
	 end,
    F3 = fun() -> % fail
		 create_x(G, parent),
		 create_x_in_y(G, child, parent),
		 delete_x(G, parent) % fail since parent has assignment
	 end,
    F4 = fun() -> % fail
		 create_x(G, parent),
		 create_x_in_y(G, child, parent),
		 delete_assign(G, parent, child) % bad reversed arguments
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2)),
     ?_assertNotEqual(ok, t(F3)),
     ?_assertNotEqual(ok, t(F4))].

tst_assign() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b]],
		 create_assign(G, a, b)
	 end,
    F2 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b]],
		 create_assign(G, a, b),
		 delete_assign(G, a, b)
	 end,
    F3 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b]],
		 create_assign(G, a, b),
		 delete_assign(G, a, b),
		 delete_x(G, a),
		 delete_x(G, b)
	 end,
    F4 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b]],
		 create_assign(G, a, b),
		 delete_x(G, a) % has reference
	 end,
    F5 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b]],
		 create_assign(G, a, b),
		 delete_x(G, b) % has reference
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertEqual(ok, t(F2)),
     ?_assertEqual(ok, t(F3)),
     ?_assertNotEqual(ok, t(F4)),
     ?_assertNotEqual(ok, t(F5))].

tst_create_rop() ->
    F1 = fun() -> % success
		 Op = #op{id = a, value = x},
		 create_rop(Op)
	 end,
    [?_assertEqual(ok, t(F1))].
    
tst_delete_rop() ->
    F1 = fun() -> % success
		 Op = #op{id = a, value = x},
		 create_rop(Op),
		 delete_rop(a)
	 end,
    [?_assertEqual(ok, t(F1))].

tst_create_aop() ->
    F1 = fun() -> % success
		 Op = #op{id = a, value = x},
		 create_aop(Op)
	 end,
    [?_assertEqual(ok, t(F1))].
    
tst_delete_aop() ->
    F1 = fun() -> % success
		 Op = #op{id = a, value = x},
		 create_aop(Op),
		 delete_aop(a)
	 end,
    [?_assertEqual(ok, t(F1))].

tst_create_ar() ->
    F1 = fun() -> % success
		 AR = #ar{id = a, value = x},
		 create_ar(AR)
	 end,
    [?_assertEqual(ok, t(F1))].
 
tst_create_arset() ->
    AR1 = #ar{id = k, value = x},
    AR2 = #ar{id = l, value = y},
    AR3 = #ar{id = m, value = z},
    F1 = fun() -> % success
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]])
	 end,
    F2 = fun() -> % success
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]])
	 end,
    F3 = fun() -> % fail
		 [create_ar(AR) || AR <- [AR1, AR2]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]])
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertEqual(ok, t(F2)),
     ?_assertNotEqual(ok, t(F3))].

tst_delete_ar() ->
    AR1 = #ar{id = k, value = x},
    AR2 = #ar{id = l, value = y},
    AR3 = #ar{id = m, value = z},
    F1 = fun() -> % success
		 AR = #ar{id = a, value = x},
		 create_ar(AR),
		 delete_ar(a)
	 end,
    F2 = fun() -> % fail
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 delete_ar(k)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2))].

tst_delete_arset() ->
    AR1 = #ar{id = k, value = x},
    AR2 = #ar{id = l, value = y},
    AR3 = #ar{id = m, value = z},
    F1 = fun() -> % success
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 delete_arset(p),
		 [delete_ar(AR_id) || AR_id <- [k, l, m]],
		 ok
	 end,
    F2 = fun() -> % fail
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 delete_arset(p),
		 [delete_ar(AR_id) || AR_id <- [k, l, m]],
		 ok
	 end,
    F3 = fun() -> % fail
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 delete_arset(p),
		 delete_arset(p)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2)),
     ?_assertNotEqual(ok, t(F3))].

tst_create_atset() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atset(p, [Id || Id <- [a, b, c]], atiset)
	 end,
    F2 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atset(p, [Id || Id <- [a, b, c]], atiset),
		 create_atset(p, [Id || Id <- [a, b, c]], atiset)
	 end,
    F3 = fun() -> % success
		 create_atset(p, [], atiset)
	 end,
    F4 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b]],
		 create_atset(p, [Id || Id <- [a, b, c]], atiset)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertEqual(ok, t(F2)),
     ?_assertEqual(ok, t(F3)),
     ?_assertNotEqual(ok, t(F4))].
   
tst_create_atiset() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atiset(p, [Id || Id <- [a, b, c]])
	 end,
    [?_assertEqual(ok, t(F1))].

tst_create_ateset() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_ateset(p, [Id || Id <- [a, b, c]])
	 end,
    [?_assertEqual(ok, t(F1))].

tst_delete_atset() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atset(p, [Id || Id <- [a, b, c]], atiset),
		 delete_atset(p, atiset)
	 end,
    F2 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atset(p, [], atiset),
		 delete_atset(p, atiset)
	 end,
    F3 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atset(p, [Id || Id <- [a, b, c]], atiset),
		 create_atset(p, [Id || Id <- [a, b, c]], atiset),
		 delete_atset(p, atiset),
		 delete_atset(p, atiset)
	 end,
    F4 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atset(p, [Id || Id <- [a, b, c]], atiset),
		 delete_atset(p, atiset),
		 delete_atset(p, atiset)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertEqual(ok, t(F2)),
     ?_assertEqual(ok, t(F3)),
     ?_assertNotEqual(ok, t(F4))].

tst_delete_atiset() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atiset(p, [Id || Id <- [a, b, c]]),
		 delete_atiset(p)
	 end,
    F2 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_atiset(p, [Id || Id <- [a, b, c]]),
		 delete_ateset(p) % Wrong set
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2))].
    
tst_delete_ateset() ->
    G = digraph:new([acyclic]),
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_ateset(p, [Id || Id <- [a, b, c]]),
		 delete_ateset(p)
	 end,
    F2 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b, c]],
		 create_ateset(p, [Id || Id <- [a, b, c]]),
		 delete_atiset(p) % Wrong set
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2))].

tst_create_seq() ->
    F1 = fun() -> % success
		 Seq = [1, 2, 3, 4],
		 create_seq(a, Seq, pattern)
	 end,
    [?_assertEqual(ok, t(F1))].
    
tst_create_pattern() ->
    F1 = fun() -> % success
		 Pattern = [1, 2, 3, 4],
		 create_pattern(a, Pattern)
	 end,
    [?_assertEqual(ok, t(F1))].
    
tst_create_response() ->
    F1 = fun() -> % success
		 Response = [1, 2, 3, 4],
		 create_response(a, Response)
	 end,
    [?_assertEqual(ok, t(F1))].
    
tst_delete_pattern() ->
    F1 = fun() -> % success
		 Pattern = [1, 2, 3, 4],
		 create_pattern(a, Pattern),
		 delete_pattern(a)
	 end,
    [?_assertEqual(ok, t(F1))].
    
tst_delete_response() ->
    F1 = fun() -> % success
		 Response = [1, 2, 3, 4],
		 create_response(a, Response),
		 delete_response(a)
	 end,
    [?_assertEqual(ok, t(F1))].
    
tst_allocate_id() ->
    Id0 = allocate_id(),
    Id1 = allocate_id(),
    Id2 = allocate_id(t),
    Id3 = allocate_id(t),
    Id4 = allocate_id(t, [a, b, c]),
    Id5 = allocate_id(t, [a, b, c]),
    Id6 = allocate_id(t, [b, c, a]),
    [?_assertNotEqual(Id0, Id1),
     ?_assertNotEqual(Id2, Id3),
     ?_assertEqual(Id4, Id5),
     ?_assertEqual(Id4, Id6)].

tst_create_assoc() ->
    G = digraph:new([acyclic]),
    AR1 = #ar{id = k, value = x},
    AR2 = #ar{id = l, value = y},
    AR3 = #ar{id = m, value = z},
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_assoc(a, p, b)
	 end,
    [?_assertEqual(ok, t(F1))].
    
tst_delete_assoc() ->
    G = digraph:new([acyclic]),
    AR1 = #ar{id = k, value = x},
    AR2 = #ar{id = l, value = y},
    AR3 = #ar{id = m, value = z},
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_assoc(a, p, b),
		 delete_assoc(a, p, b)
	 end,
    F2 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_assoc(a, p, b),
		 delete_arset(p)
	 end,
    F3 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_assoc(a, p, b),
		 delete_x(G, a)
	 end,
    F4 = fun() -> % fail
		 [create_x(G, ID) || ID <- [a, b, c, d]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_assoc(a, p, b),
		 create_assoc(c, p, d),
		 delete_assoc(a, p, b),
		 delete_arset(p)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2)),
     ?_assertNotEqual(ok, t(F3)),
     ?_assertNotEqual(ok, t(F4))].

tst_create_prohib() ->
    G = digraph:new([acyclic]),
    AR1 = #ar{id = k, value = x},
    AR2 = #ar{id = l, value = y},
    AR3 = #ar{id = m, value = z},
    F1 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c, d, e]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_atiset(x, [Id || Id <- [b, c]]),
		 create_ateset(y, [Id || Id <- [d, e]]),
		 create_prohib(a, p, x, y, u_deny_conj),
		 delete_prohib(a, p, x, y, u_deny_conj),
		 delete_atiset(x),
		 delete_ateset(y),
		 delete_arset(p)
	 end,
    F2 = fun() -> % success
		 [create_x(G, ID) || ID <- [a, b, c, d, e]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_atiset(w, [Id || Id <- [b, c]]),
		 create_atiset(x, []),
		 create_ateset(y, [Id || Id <- [d, e]]),
		 create_ateset(z, []),
		 create_prohib(a, p, w, y, u_deny_conj),
		 create_prohib(a, p, x, z, u_deny_conj)
	 end,
    F3 = fun() -> % FAIL
		 [create_x(G, ID) || ID <- [a, b, c, d, e]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_atiset(x, [Id || Id <- [b, c]]),
		 create_ateset(y, [Id || Id <- [d, e]]),
		 create_prohib(a, p, x, y, u_deny_conj),
		 delete_arset(p)
	 end,
    F4 = fun() -> % FAIL
		 [create_x(G, ID) || ID <- [a, b, c, d, e]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_atiset(x, [Id || Id <- [b, c]]),
		 create_ateset(y, [Id || Id <- [d, e]]),
		 create_prohib(a, p, x, y, u_deny_conj),
		 delete_atiset(x)
	 end,
    F5 = fun() -> % FAIL
		 [create_x(G, ID) || ID <- [a, b, c, d, e]],
		 [create_ar(AR) || AR <- [AR1, AR2, AR3]],
		 create_arset(p, [AR_id || #ar{id = AR_id} <- [AR1, AR2, AR3]]),
		 create_atiset(x, [Id || Id <- [b, c]]),
		 create_ateset(y, [Id || Id <- [d, e]]),
		 create_prohib(a, p, x, y, u_deny_conj),
		 delete_ateset(y)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertEqual(ok, t(F2)),
     ?_assertNotEqual(ok, t(F3)),
     ?_assertNotEqual(ok, t(F4)),
     ?_assertNotEqual(ok, t(F5))].
    
tst_create_oblig() ->
    G = digraph:new([acyclic]),
    P1 = #pattern{id = k, value = 1},
    P2 = #pattern{id = l, value = 2},
    P3 = #pattern{id = m, value = 3},
    R1 = #response{id = n, value = 4},
    R2 = #response{id = o, value = 5},
    R3 = #response{id = p, value = 6},
    F1 = fun() -> % success
		 create_x(G, a),
		 create_pattern(p, [Id || #pattern{id = Id} <- [P1, P2, P3]]),
		 create_response(q, [Id || #response{id = Id} <- [R1, R2, R3]]), 
		 create_oblig(a, p, q)
	 end,
    F2 = fun() -> % fail
		 create_x(G, a),
		 create_pattern(p, [Id || #pattern{id = Id} <- [P1, P2, P3]]),
		 create_response(q, [Id || #response{id = Id} <- [R1, R2, R3]]), 
		 create_oblig(a, p, q),
		 delete_pattern(p)
	 end,
    F3 = fun() -> % fail
		 create_x(G, a),
		 create_pattern(p, [Id || #pattern{id = Id} <- [P1, P2, P3]]),
		 create_response(q, [Id || #response{id = Id} <- [R1, R2, R3]]), 
		 create_oblig(a, p, q),
		 delete_response(q)
	 end,
    F4 = fun() -> % fail
		 create_x(G, a),
		 create_pattern(p, [Id || #pattern{id = Id} <- [P1, P2, P3]]),
		 create_response(q, [Id || #response{id = Id} <- [R1, R2, R3]]), 
		 create_oblig(a, p, q),
		 delete_x(G, a)
	 end,
    [?_assertEqual(ok, t(F1)),
     ?_assertNotEqual(ok, t(F2)),
     ?_assertNotEqual(ok, t(F3)),
     ?_assertNotEqual(ok, t(F4))].

tst_delete_oblig() ->
    G = digraph:new([acyclic]),
    P1 = #pattern{id = k, value = 1},
    P2 = #pattern{id = l, value = 2},
    P3 = #pattern{id = m, value = 3},
    R1 = #response{id = n, value = 4},
    R2 = #response{id = o, value = 5},
    R3 = #response{id = p, value = 6},
    F1 = fun() -> % success
		 create_x(G, a),
		 create_pattern(p, [Id || #pattern{id = Id} <- [P1, P2, P3]]),
		 create_response(q, [Id || #response{id = Id} <- [R1, R2, R3]]), 
		 create_oblig(a, p, q),
		 delete_oblig(a, p, q)
	 end,
    [?_assertEqual(ok, t(F1))].

tst_create_p() ->    
    PU = [],
    P = spawn_link(fun() -> receive _ -> ok end end),
    [?_assertEqual([#process_user{u = u, p = P}], create_p(P, u, PU))].
    
tst_delete_p() ->    
    P = spawn_link(fun() -> receive _ -> ok end end),
    PU = create_p(P, u, []),
    [?_assertEqual([], delete_p(P, PU))].
    
tst_pumapping() ->
    P = spawn_link(fun() -> receive _ -> ok end end),
    PU = create_p(P, u, []),
    PU1 = delete_pumapping(P, u, PU),
    PU2 = create_pumapping(P, v, PU1),
    [?_assertEqual([#process_user{u = undefined, p = P}], PU1),
     ?_assertEqual([#process_user{u = v, p = P}], PU2)].

-endif.
