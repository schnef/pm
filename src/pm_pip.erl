-module(pm_pip).

%%% @doc

-include_lib("eunit/include/eunit.hrl").
-include("pm.hrl").

%% API
-export([create_x_in_y/3, create_x/2, delete_x/2, % create_p/2, 
	 create_rop/1, create_aop/1, create_ar/1, create_arset/2,
	 delete_rop/1, delete_aop/1, delete_ar/1, delete_arset/1,
	 create_atiset/2, create_ateset/2, create_pattern/2, create_response/2,
	 delete_atiset/1, delete_ateset/1, delete_pattern/1, delete_response/1,
	 allocate_id/0, allocate_id/1,
	 create_p/3, delete_p/2, 
	 create_pumapping/3, create_assign/3, create_assoc/3, create_prohib/5, create_oblig/3,
	 delete_pumapping/3, delete_assign/3, delete_assoc/3, delete_prohib/5, delete_oblig/3,
	 process_user/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%% C.2 Element Creation Commands
-spec create_x_in_y(G :: digraph:graph(), X :: pm:id(), Y :: pm:id()) -> ok | no_return().
create_x_in_y(G, X, Y) ->
    [#pe{vertex = Vy} = PEY] = mnesia:read(pe, Y, write),
    case digraph:add_vertex(G) of
	{error, Reason} ->
	    erlang:error(Reason);
	Vx ->
	    case digraph:add_edge(G, Vx, Vy, Y) of
		{error, Reason} ->
		    erlang:error(Reason);
		Edge ->
		    %% ref_cnt for X is 1 because of the assignment
		    mnesia:write(#pe{id = X, vertex = Vx, ref_cnt = 1}),
		    mnesia:write(PEY#pe{ref_cnt = PEY#pe.ref_cnt + 1}),
		    mnesia:write(#assign{a = X, b = Y, edge = Edge})
	    end
    end.
    
-spec create_x(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
%% @doc add a policy element x to the policy representation
create_x(G, X) ->
    case digraph:add_vertex(G) of
	{error, Reason} ->
	    erlang:error(Reason);
	Vx ->
	    mnesia:write(#pe{id = X, vertex = Vx, ref_cnt = 0})
    end.

%% C.3 Element Deletion Commands
-spec delete_x(G :: digraph:graph(), X :: pm:id()) -> ok | no_return().
%% @doc delete a policy element x from the policy representation
delete_x(G, X) ->
    %% ref_cnt must be zero for this action!  
    [#pe{vertex = V, ref_cnt = 0} = PE] = mnesia:read(pe, X, write),
    digraph:del_vertex(G, V),
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
create_arset(ARset, AR_ids) ->
    case mnesia:read(arset, ARset, read) of
	[] ->
	    [begin 
		 [AR] = mnesia:read(ar, AR_id, write),
		 mnesia:write(AR#ar{ref_cnt = AR#ar.ref_cnt + 1})
	     end || AR_id <- AR_ids],
	    Set = #set{id = ARset, value = sets:from_list(AR_ids), ref_cnt = 0, inst_cnt = 1},
	    mnesia:write(arset, Set, write);
	[#set{inst_cnt = N} = Set] ->
	    mnesia:write(arset, Set#set{inst_cnt = N + 1}, write)
    end.
    
-spec create_atiset(ATIset :: pm:id(), ATIs :: list(pm:at())) -> ok | no_return().
%% @doc add a set of inclusion policy elements denoted by a referent attribute to the representation
create_atiset(ATIset, ATI_ids) ->
    create_atset(ATIset, ATI_ids, atiset).

-spec create_ateset(ATEset :: pm:id(), ATE_ids :: list(pm:at())) -> ok | no_return().
%% @doc add a set of exclusion policy elements denoted by a referent attribute to the representation
create_ateset(ATEset, ATE_ids) ->
    create_atset(ATEset, ATE_ids, ateset).

%% TODO: merge create_arset and create_atset into a single
%% function. These are really similar.
create_atset(ATset, AT_ids, ATset_table) ->
    case mnesia:read(ATset_table, ATset, read) of
	[] ->
	    [begin 
		 [PE] = mnesia:read(pe, AT_id, write),
		 mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt + 1})
	     end || AT_id <- AT_ids],
	    Set = #set{id = ATset, value = AT_ids, ref_cnt = 0, inst_cnt = 1},
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
    zuuid:v4().

-spec allocate_id(Values :: nonempty_list(term())) -> pm:id().
%% @doc allocate an identifier for use in the PM framework based on a
%% list of values. This is used for determining the value of a set
%% id. Instead of generating new ids for the same piece of data, data
%% is reused and reference counted.

%% TODO: we use a 32 bit hash (maybe 27 bits) to calculate a key based
%% on the value(s) passed to the allocate_id function. Is the range
%% sufficient? Use hash functions from the crypto lib?
allocate_id(Values)  when is_list(Values) ->
    %% TODO: can range be set to its default of 0 .. 2^27 - 1? Once
    %% set, you can _NOT_ change the range anymore since the result of
    %% the hash differs!
    %% erlang:phash2(Values). 
    erlang:phash2(Values, 16#100000000).

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
    	[#set{value = AT_ids, inst_cnt = 1, ref_cnt = 0}] ->
    	    [begin 
    		 [PE] = mnesia:read(pe, AT_id, write),
    		 mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt - 1})
    	     end || AT_id <- AT_ids],
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
    [#pe{vertex = Vx} = PEX] = mnesia:read(pe, X, write),
    [#pe{vertex = Vy} = PEY] = mnesia:read(pe, Y, write),
    case digraph:add_edge(G, Vx, Vy, Y) of
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
    mnesia:write(#assoc{a = X, b = Y, c = Z}).

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
    mnesia:write(Prohib_table, #prohib{a = W, b = X, c = Y, d = Z}, write).

-spec create_oblig(X :: pm:id(), Y :: pm:id(), Z :: pm:id()) -> ok | no_return().
%% @doc add tuple (x, y, z) to the obligation relation
create_oblig(X, Y, Z) ->
    [PE] = mnesia:read(pe, X, write),
    [Pattern] = mnesia:read(pattern, Y, write),
    [Response] = mnesia:read(response, Z, write),
    mnesia:write(PE#pe{ref_cnt = PE#pe.ref_cnt + 1}),
    mnesia:write(pattern, Pattern#seq{ref_cnt = Pattern#seq.ref_cnt + 1}, write),
    mnesia:write(response, Response#seq{ref_cnt = Response#seq.ref_cnt + 1}, write),
    mnesia:write(#oblig{a = X, b = Y, c = Z}).

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
    [Row] = mnesia:match_object(#assoc{a = X, b = Y, c = Z, _ = '_'}),
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
    [Row] = mnesia:match_object(Prohib_table, #prohib{a = W, b = X, c = Y, d = Z, _ = '_'}, write),
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
    [Row] = mnesia:match_object(#oblig{a = X, b = Y, _ = '_'}),
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

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(EUNIT).

%% Tests are wrapped in a Mnesia transaction and all are aborted to
%% revert all changes in the database. This should prevent polluting
%% an existing database.

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
    Id1 = allocate_id(),
    Id2 = allocate_id(),
    Id3 = allocate_id([a, b, c]),
    Id4 = allocate_id([a, b, c]),
    Id5 = allocate_id([b, c, a]),
    [?_assertNotEqual(Id1, Id2),
     ?_assertEqual(Id3, Id4),
     ?_assertNotEqual(Id3, Id5)].

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
