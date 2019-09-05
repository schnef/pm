-module(pm_pap).

%%% @doc We mainly use a server here to make access
%%% sequential. Multiple sources of administrative commands should not
%%% interfere.

%% TODO: Review deleting entities and relations. Maybe we should check
%% to see if entites exist in Mnesia in the API. In the API we still
%% know what tables should be checked, which often is not the case
%% within the server's handlers. Also, checking before calling the
%% server will prevent transaction abortions.

%% TODO: make error values consistent. Some functions return varying
%% error messages depending on the type of error that occurs. Some
%% errors are caught in the API and others are returned by the server,
%% making the error a different type. See the tests for prohibitions
%% for an example.

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include("pm.hrl").

%% API
-export([start_link/0, stop/0, get_digraph/0,
	 c_u_in_ua/2, c_ua_in_ua/2, c_ua_in_pc/2, c_u_to_ua/2, c_ua_to_ua/2, c_ua_to_pc/2,
	 c_o_in_oa/2, c_oa_in_oa/2, c_oa_in_pc/2, c_o_to_oa/2, c_oa_to_oa/2, c_oa_to_pc/2,
	 c_pc/1, c_assoc/3, c_oblig/3,
	 c_conj_uprohib/4, c_conj_pprohib/4, c_conj_uaprohib/4,
	 c_disj_uprohib/4, c_disj_pprohib/4, c_disj_uaprohib/4,
	 eval_pattern/2, eval_response/2,
	 d_u_in_ua/2, d_u_to_ua/2, d_ua_in_ua/2, d_ua_to_ua/2, d_ua_in_pc/2, d_ua_to_pc/2,
	 d_o_in_oa/2, d_o_to_oa/2, d_oa_in_oa/2, d_oa_to_oa/2, d_oa_in_pc/2, d_oa_to_pc/2,
	 d_pc/1, d_assoc/3, d_oblig/3,
	 d_conj_uprohib/4, d_conj_pprohib/4, d_conj_uaprohib/4,
	 d_disj_uprohib/4, d_disj_pprohib/4, d_disj_uaprohib/4]).

-export([users/1, objects/1, elements/1, icap/1, iae/1, disj_range/2, conj_range/2]).

-export([rebuild/0]).

%% Server name registery API
-export([register_p/2, unregister_p/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {g, pu :: [#process_user{}]}).

%%%===================================================================
%%% API
%%%===================================================================

%% Administrative Routines
%% D.1 Relation Formation Routines
-spec c_u_in_ua(U, UA) -> Result when
      U :: pm:u(),
      UA :: pm:ua(),
      Result :: {ok, pm:u()} | {error, Reason :: term()}.
%% @doc create a u assigned to ua
c_u_in_ua(#u{} = U, #ua{} = UA) ->
    gen_server:call(?SERVER, {c_u_in_ua, U, UA});
c_u_in_ua(U, UA) ->
    {error, {badarg, {U, UA}}}.

-spec c_ua_in_ua(UA1, UA2) -> Result when
      UA1 :: pm:ua(),
      UA2 :: pm:ua(),
      Result :: {ok, pm:ua()} | {error, Reason :: term()}.
%% @doc create a ua, x, assigned to ua
c_ua_in_ua(#ua{} = UA1, #ua{} = UA2) ->
    gen_server:call(?SERVER, {c_ua_in_ua, UA1, UA2});
c_ua_in_ua(UA1, UA2) ->
    {error, {badarg, {UA1, UA2}}}.

-spec c_ua_in_pc(UA, PC) -> Result when
      UA :: pm:ua(),
      PC :: pm:pc(),
      Result :: {ok, pm:ua()} | {error, Reason :: term()}.
%% @doc create a ua in pc
c_ua_in_pc(#ua{} = UA, #pc{} = PC) ->
    gen_server:call(?SERVER, {c_ua_in_pc, UA, PC});
c_ua_in_pc(UA, PC) ->
    {error, {badarg, {UA, PC}}}.

-spec c_o_in_oa(O, OA) -> Result when
      O :: pm:o(),
      OA :: pm:oa(),
      Result :: {ok, pm:o()} | {error, Reason :: term()}.
%% @doc create a o assigned to oa
c_o_in_oa(#o{} = O, #oa{} = OA) ->
    gen_server:call(?SERVER, {c_o_in_oa, O, OA});
c_o_in_oa(O, OA) ->
    {error, {badarg, {O, OA}}}.

-spec c_oa_in_oa(OA1, OA2) -> Result when
      OA1 :: pm:oa(),
      OA2 :: pm:oa(),
      Result :: {ok, pm:oa()} | {error, Reason :: term()}.
%% @doc create a oa, x, assigned to oa
c_oa_in_oa(#oa{} = OA1, #oa{} = OA2) ->
    gen_server:call(?SERVER, {c_oa_in_oa, OA1, OA2});
c_oa_in_oa(OA1, OA2) ->
    {error, {badarg, {OA1, OA2}}}.

-spec c_oa_in_pc(OA, PC) -> Result when
      OA :: pm:oa(),
      PC :: pm:pc(),
      Result :: {ok, pm:oa()} | {error, Reason :: term()}.
%% @doc create a oa in pc
c_oa_in_pc(#oa{} = OA, #pc{} = PC) ->
    gen_server:call(?SERVER, {c_oa_in_pc, OA, PC});
c_oa_in_pc(OA, PC) ->
    {error, {badarg, {OA, PC}}}.

-spec c_pc(PC) -> Result when
      PC :: pm:pc(),
      Result :: {ok, pm:pc()} | {error, Reason :: term()}.
%% @doc create a policy class
c_pc(#pc{} = PC) ->
    gen_server:call(?SERVER, {c_pc, PC});
c_pc(PC) ->
    {error, {badarg, PC}}.

-spec c_u_to_ua(U, UA) -> Result when
      U :: pm:u(),
      UA :: pm:ua(),
      Result :: ok | {error, Reason :: term()}.
%% @doc create an assignment (u, ua)
c_u_to_ua(#u{id = X}, #ua{id = Y}) ->
    gen_server:call(?SERVER, {c_assign, X, Y});
c_u_to_ua(U, UA) ->
    {error, {badarg, {U, UA}}}.

-spec c_ua_to_ua(UAfr, UAto) -> Result when
      UAfr :: pm:ua(),
      UAto :: pm:ua(),
      Result :: ok | {error, Reason :: term()}.
%% @doc create an assignment (uafr, uato)
c_ua_to_ua(#ua{id = X}, #ua{id = Y}) ->
    gen_server:call(?SERVER, {c_assign, X, Y});
c_ua_to_ua(UA1, UA2) ->
    {error, {badarg, {UA1, UA2}}}.

-spec c_ua_to_pc(UA, PC) -> Result when
      UA :: pm:ua(),
      PC :: pm:pc(),
      Result :: ok | {error, Reason :: term()}.
%% @doc create an assignment (ua, pc)
c_ua_to_pc(#ua{id = X}, #pc{id = Y}) ->
    gen_server:call(?SERVER, {c_assign, X, Y});
c_ua_to_pc(UA, PC) ->
    {error, {badarg, {UA, PC}}}.

-spec c_o_to_oa(O, OA) -> Result when
      O :: pm:o(),
      OA :: pm:oa(),
      Result :: ok | {error, Reason :: term()}.
%% @doc create an assignment (o, oa)
c_o_to_oa(#o{id = X}, #oa{id = Y}) ->
    gen_server:call(?SERVER, {c_assign, X, Y});
c_o_to_oa(O, OA) ->
    {error, {badarg, {O, OA}}}.

-spec c_oa_to_oa(OAfr, OAto) -> Result when
      OAfr :: pm:oa(),
      OAto :: pm:oa(),
      Result :: ok | {error, Reason :: term()}.
%% @doc create an assignment (oafr, oato)
c_oa_to_oa(#oa{id = X}, #oa{id = Y}) ->
    gen_server:call(?SERVER, {c_assign, X, Y});
c_oa_to_oa(OA1, OA2) ->
    {error, {badarg, {OA1, OA2}}}.

-spec c_oa_to_pc(OA, PC) -> Result when
      OA :: pm:oa(),
      PC :: pm:pc(),
      Result :: ok | {error, Reason :: term()}.
%% @doc create an assignment (oa, pc)
c_oa_to_pc(#oa{id = X}, #pc{id = Y}) ->
    gen_server:call(?SERVER, {c_assign, X, Y});
c_oa_to_pc(OA, PC) ->
    {error, {badarg, {OA, PC}}}.

-spec c_assoc(UA, ARs, AT) -> Result when
      UA :: pm:ua(),
      ARs :: nonempty_list(pm:ar()),
      AT :: pm:ua() | pm:o() | pm:oa(),
      Result :: ok | {error, Reason :: term()}.
%% @doc create an association (ua, ars, at)
c_assoc(UA, ARs, #ua{id = Z}) ->
    c_assoc(UA, ARs, Z);
c_assoc(UA, ARs, #o{id = Z}) ->
    c_assoc(UA, ARs, Z);
c_assoc(UA, ARs, #oa{id = Z}) ->
    c_assoc(UA, ARs, Z);
c_assoc(#ua{id = X}, ARs, Z) when ARs =/= [] ->
    AR_ids = [(fun(#ar{id = AR_id}) -> AR_id end)(AR) || AR <- ARs],
    case gen_server:call(?SERVER, {c_assoc, X, AR_ids, Z}) of
	{ok, ARset} ->
	    {ok, #association{ua = X, arset = ARset, at = Z}};
	Error ->
	    Error
    end;
c_assoc(UA, ARs, AT) ->
    {error, {badarg, {UA, ARs, AT}}}.

%% TODO: -specs should state that ATIset and ATEset should contain the
%% same attribute types and not, for example, the ATIset containing
%% UA's and the ATEset containing O's
-spec c_conj_uprohib(U :: pm:u(), ARs, ATIs, ATEs) -> Result when
      ARs :: nonempty_list(pm:ar()),
      ATIs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      ATEs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      Result :: ok | {error, Reason :: term()}.
%% @doc create a conjunctive user prohibition
c_conj_uprohib(#u{id = W}, ARs, ATIs, ATEs) ->
    case c_prohib(W, ARs, ATIs, ATEs, u_deny_conj) of
	{ok, {ARset, ATIset, ATEset}} ->
	    {ok, #u_deny_conj{u = W, arset = ARset, atiset = ATIset, ateset = ATEset}};
	Error ->
	    Error
    end;
c_conj_uprohib(U, ARs, ATIs, ATEs) ->
    {error, {badarg, {U, ARs, ATIs, ATEs}}}.

-spec c_conj_pprohib(P :: pm:p(), ARs, ATIs, ATEs) -> Result when
      ARs :: nonempty_list(pm:ar()),
      ATIs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      ATEs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      Result :: ok | {error, Reason :: term()}.
%% @doc create a conjunctive process prohibition
%% TODO: isn't the is_pid(P) a little to defensive?
c_conj_pprohib(P, ARs, ATIs, ATEs) when is_pid(P) ->
    case c_prohib(P, ARs, ATIs, ATEs, p_deny_conj) of
	{ok, {ARset, ATIset, ATEset}} ->
	    {ok, #p_deny_conj{p = P, arset = ARset, atiset = ATIset, ateset = ATEset}};
	Error ->
	    Error
    end;
c_conj_pprohib(P, ARs, ATIs, ATEs) ->
    {error, {badarg, {P, ARs, ATIs, ATEs}}}.

-spec c_conj_uaprohib(UA :: pm:ua(), ARs, ATIs, ATEs) -> Result when
      ARs :: nonempty_list(pm:ar()),
      ATIs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      ATEs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      Result :: ok | {error, Reason :: term()}.
%% @doc create a conjunctive user attribute prohibition
c_conj_uaprohib(#ua{id = W}, ARs, ATIs, ATEs) ->
    case c_prohib(W, ARs, ATIs, ATEs, ua_deny_conj) of
	{ok, {ARset, ATIset, ATEset}} ->
	    {ok, #ua_deny_conj{ua = W, arset = ARset, atiset = ATIset, ateset = ATEset}};
	Error ->
	    Error
    end;
c_conj_uaprohib(UA, ARs, ATIs, ATEs) ->
    {error, {badarg, {UA, ARs, ATIs, ATEs}}}.

-spec c_disj_uprohib(U :: pm:u(), ARs, ATIs, ATEs) -> Result when
      ARs :: nonempty_list(pm:ar()),
      ATIs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      ATEs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      Result :: ok | {error, Reason :: term()}.
%% @doc create a disjunctive user prohibition
c_disj_uprohib(#u{id = W}, ARs, ATIs, ATEs) ->
    case c_prohib(W, ARs, ATIs, ATEs, u_deny_disj) of
	{ok, {ARset, ATIset, ATEset}} ->
	    {ok, #u_deny_disj{u = W, arset = ARset, atiset = ATIset, ateset = ATEset}};
	Error ->
	    Error
    end;
c_disj_uprohib(U, ARs, ATIs, ATEs) ->
    {error, {badarg, {U, ARs, ATIs, ATEs}}}.

-spec c_disj_pprohib(P :: pm:p(), ARs, ATIs, ATEs) -> Result when
      ARs :: nonempty_list(pm:ar()),
      ATIs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      ATEs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      Result :: ok | {error, Reason :: term()}.
%% @doc create a disjunctive process prohibition
%% TODO: isn't the is_pid(P) a little to defensive?
c_disj_pprohib(P, ARs, ATIs, ATEs) when is_pid(P) ->
    case c_prohib(P, ARs, ATIs, ATEs, p_deny_disj) of
	{ok, {ARset, ATIset, ATEset}} ->
	    {ok, #p_deny_disj{p = P, arset = ARset, atiset = ATIset, ateset = ATEset}};
	Error ->
	    Error
    end;
c_disj_pprohib(P, ARs, ATIs, ATEs) ->
    {error, {badarg, {P, ARs, ATIs, ATEs}}}.

-spec c_disj_uaprohib(UA :: pm:ua(), ARs, ATIs, ATEs) -> Result when
      ARs :: nonempty_list(pm:ar()),
      ATIs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      ATEs :: [pm:ua()] | [pm:o()] | [pm:oa()],
      Result :: ok | {error, Reason :: term()}.
%% @doc create a disjunctive user attribute prohibition
c_disj_uaprohib(#ua{id = W}, ARs, ATIs, ATEs) ->
    case c_prohib(W, ARs, ATIs, ATEs, ua_deny_disj) of
	{ok, {ARset, ATIset, ATEset}} ->
	    {ok, #ua_deny_disj{ua = W, arset = ARset, atiset = ATIset, ateset = ATEset}};
	Error ->
	    Error
    end;
c_disj_uaprohib(UA, ARs, ATIs, ATEs) ->
    {error, {badarg, {UA, ARs, ATIs, ATEs}}}.

%% @private
c_prohib(_W, [], _ATIs, _ATEs, _Prohib) ->
    {error, badvalue};
c_prohib(_W, _ARs, [], [], _Prohib) ->
    {error, badvalue};
c_prohib(W, [#ar{} | _] = ARs, ATIs, ATEs, Prohib) ->
    AR_ids = [(fun(#ar{id = AR_id}) -> AR_id end)(AR) || AR <- ARs],
    c_prohib(W, AR_ids, ATIs, ATEs, Prohib);
c_prohib(W, AR_ids, [] = _ATIs, [#ua{} | _] = ATEs, Prohib) ->
    ATE_ids = [(fun(#ua{id = AT_id}) -> AT_id end)(AT) || AT <- ATEs],
    gen_server:call(?SERVER, {c_prohib, W, AR_ids, [], ATE_ids, Prohib});
c_prohib(W, AR_ids, [] = _ATIs, ATEs, Prohib) ->
    ATE_ids = [(fun(#oa{id = AT_id}) -> AT_id;
		   (#o{id = AT_id}) -> AT_id end)(AT) || AT <- ATEs],
    gen_server:call(?SERVER, {c_prohib, W, AR_ids, [], ATE_ids, Prohib});
c_prohib(W, AR_ids, [#ua{} | _] = ATIs, ATEs, Prohib) ->
    ATI_ids = [(fun(#ua{id = AT_id}) -> AT_id end)(AT) || AT <- ATIs],
    ATE_ids = [(fun(#ua{id = AT_id}) -> AT_id end)(AT) || AT <- ATEs],
    gen_server:call(?SERVER, {c_prohib, W, AR_ids, ATI_ids, ATE_ids, Prohib});
c_prohib(W, AR_ids, ATIs, ATEs, Prohib) ->
    ATI_ids = [(fun(#oa{id = AT_id}) -> AT_id;
		   (#o{id = AT_id}) -> AT_id end)(AT) || AT <- ATIs],
    ATE_ids = [(fun(#oa{id = AT_id}) -> AT_id;
		   (#o{id = AT_id}) -> AT_id end)(AT) || AT <- ATEs],
    gen_server:call(?SERVER, {c_prohib, W, AR_ids, ATI_ids, ATE_ids, Prohib}).

-spec eval_pattern(P, Pattern) -> Result when
      P :: pid(),
      Pattern :: nonempty_list(pm:pattern()),
      Result :: boolean().
%% @doc function that evaluates the correctness of a logical expression of an event pattern
eval_pattern(P, Patterns) when is_pid(P), Patterns =/= [] ->
    Pattern_ids = [(fun(#pattern{id = P_id}) -> P_id end)(Pattern) || Pattern <- Patterns],
    gen_server:call(?SERVER, {eval_pattern, P, Pattern_ids});
eval_pattern(P, Pattern) ->
    {error, {badarg, {P, Pattern}}}.

-spec eval_response(P, Response) -> Result when
      P :: pid(),
      Response :: nonempty_list(pm:response()),
      Result :: boolean().
%% @doc function that evaluates of the correctness of the syntax of an obligation’s response
eval_response(P, Responses) when is_pid(P), Responses =/= [] ->
    Response_ids = [(fun(#response{id = R_id}) -> R_id end)(Response) || Response <- Responses],
    gen_server:call(?SERVER, {eval_response, P, Response_ids});
eval_response(P, Responses) ->
    {error, {badarg, {P, Responses}}}.

-spec c_oblig(P, Patterns, Responses) -> Result when
      P :: pid(),
      Patterns :: nonempty_list(pm:pattern()),
      Responses :: nonempty_list(pm:response()),
      Result :: ok | {error, Reason :: term()}.
%% @doc create an obligation
c_oblig(P, Patterns, Responses) when is_pid(P), Patterns =/= [], Responses =/= [] ->
    Pattern_ids = [(fun(#pattern{id = P_id}) -> P_id end)(Pattern) || Pattern <- Patterns],
    Response_ids = [(fun(#response{id = R_id}) -> R_id end)(Response) || Response <- Responses],
    case gen_server:call(?SERVER, {c_oblig, P, Pattern_ids, Response_ids}) of
	{ok, {U, Pattern, Response}} ->
	    {ok, #obligation{u = U, pattern = Pattern, response = Response}};
	Error ->
	    Error
    end;
c_oblig(P, Patterns, Responses) ->
    {error, {badarg, {P, Patterns, Responses}}}.

%% D.2 Relation Rescindment Routines
-spec d_u_in_ua(U, UA) -> Result when
      U :: pm:u(),
      UA :: pm:ua(),
      Result :: ok | {error, Reason :: term()}.
%% @doc Delete user assigned to this user attribute
d_u_in_ua(U, UA) ->
    gen_server:call(?SERVER, {d_u_in_ua, U#u.id, UA#ua.id}).

-spec d_ua_in_ua(UAfr, UAto) -> Result when
      UAfr :: pm:ua(),
      UAto :: pm:ua(),
      Result :: ok | {error, Reason :: term()}.
%% @doc Delete user attribute assigned to this user attribute
d_ua_in_ua(UAfr, UAto) ->
    gen_server:call(?SERVER, {d_ua_in_y, UAfr#ua.id, UAto#ua.id}).

-spec d_ua_in_pc(UA, PC) -> Result when
      UA :: pm:ua(),
      PC :: pm:pc(),
      Result :: ok | {error, Reason :: term()}.
%% @doc Delete u attribute assigned to the policy class
d_ua_in_pc(UA, PC) ->
    gen_server:call(?SERVER, {d_ua_in_y, UA#ua.id, PC#pc.id}).

-spec d_o_in_oa(O, OA) -> Result when
      O :: pm:o(),
      OA :: pm:oa(),
      Result :: ok | {error, Reason :: term()}.
%% @doc Delete object assigned to this object attribute
d_o_in_oa(O, OA) ->
    gen_server:call(?SERVER, {d_o_in_oa, O#o.id, OA#oa.id}).

-spec d_oa_in_oa(OAfr, OAto) -> Result when
      OAfr :: pm:oa(),
      OAto :: pm:oa(),
      Result :: ok | {error, Reason :: term()}.
%% @doc Delete object attribute assigned to this object attribute
d_oa_in_oa(OAfr, OAto) ->
    gen_server:call(?SERVER, {d_oa_in_y, OAfr#oa.id, OAto#oa.id}).

-spec d_oa_in_pc(OA, PC) -> Result when
      OA :: pm:oa(),
      PC :: pm:pc(),
      Result :: ok | {error, Reason :: term()}.
%% @doc Delete object attribute assigned to the policy class
d_oa_in_pc(OA, PC) ->
    gen_server:call(?SERVER, {d_oa_in_y, OA#oa.id, PC#pc.id}).

-spec d_u_to_ua(U, UA) -> Result when
      U :: pm:u(),
      UA :: pm:ua(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete the assignment from u to ua
d_u_to_ua(U, UA) ->
    gen_server:call(?SERVER, {d_assign, U#u.id, UA#ua.id}).

-spec d_ua_to_ua(UAfr, UAto) -> Result when
      UAfr :: pm:ua(),
      UAto :: pm:ua(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete the assignment from uafr to uato
d_ua_to_ua(UAfr, UAto) ->
    gen_server:call(?SERVER, {d_assign, UAfr#ua.id, UAto#ua.id}).

-spec d_ua_to_pc(UA, PC) -> Result when
      UA :: pm:ua(),
      PC :: pm:pc(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete the assignment from ua to pc
d_ua_to_pc(UA, PC) ->
    gen_server:call(?SERVER, {d_assign, UA#ua.id, PC#pc.id}).

-spec d_o_to_oa(O, OA) -> Result when
      O :: pm:o(),
      OA :: pm:oa(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete the assignment from o to oa
d_o_to_oa(O, OA) ->
    gen_server:call(?SERVER, {d_assign, O#o.id, OA#oa.id}).

-spec d_oa_to_oa(OAfr, OAto) -> Result when
      OAfr :: pm:oa(),
      OAto :: pm:oa(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete the assignment from oafr to oato
d_oa_to_oa(OAfr, OAto) ->
    gen_server:call(?SERVER, {d_assign, OAfr#oa.id, OAto#oa.id}).

-spec d_oa_to_pc(OA, PC) -> Result when
      OA :: pm:oa(),
      PC :: pm:pc(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete the assignment from oa to pc
d_oa_to_pc(OA, PC) ->
    gen_server:call(?SERVER, {d_assign, OA#oa.id, PC#pc.id}).

-spec d_pc(PC) -> Result when
      PC :: pm:pc(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete the assignment from ua to pc
d_pc(PC) ->
    gen_server:call(?SERVER, {d_pc, PC#pc.id}).

-spec d_assoc(UA, ARset, AT) -> Result when
      UA :: pm:ua(),
      ARset :: pm:id(),
      AT :: pm:oa() | pm:o() | pm:ua(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete an associationd
d_assoc(#ua{id = X}, ARset, #ua{id = Z}) ->
    d_assoc(X, ARset, Z);
d_assoc(#ua{id = X}, ARset, #o{id = Z}) ->
    d_assoc(X, ARset, Z);
d_assoc(#ua{id = X}, ARset, #oa{id = Z}) ->
    d_assoc(X, ARset, Z);
d_assoc(X, ARset, Y) ->
    gen_server:call(?SERVER, {d_assoc, X, ARset, Y}).

-spec d_conj_uprohib(U, ARset, ATIset, ATEset) -> Result when
      U :: pm:u(),
      ARset :: pm:id(),
      ATIset :: pm:id(),
      ATEset :: pm:id(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete a prohibition
d_conj_uprohib(#u{id = U_id}, ARset, ATIset, ATEset) ->
    gen_server:call(?SERVER, {d_prohib, U_id, ARset, ATIset, ATEset, u_deny_conj}).

-spec d_conj_pprohib(P, ARset, ATIset, ATEset) -> Result when
      P :: pid(),
      ARset :: pm:id(),
      ATIset :: pm:id(),
      ATEset :: pm:id(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete a prohibition
%% TODO: isn't the is_pid(P) a little to defensive?
d_conj_pprohib(P, ARset, ATIset, ATEset) when is_pid(P) ->
    gen_server:call(?SERVER, {d_prohib, P, ARset, ATIset, ATEset, p_deny_conj}).

-spec d_conj_uaprohib(UA, ARset, ATIset, ATEset) -> Result when
      UA :: pm:ua(),
      ARset :: pm:id(),
      ATIset :: pm:id(),
      ATEset :: pm:id(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete a prohibition
d_conj_uaprohib(#ua{id = UA_id}, ARset, ATIset, ATEset) ->
    gen_server:call(?SERVER, {d_prohib, UA_id, ARset, ATIset, ATEset, ua_deny_conj}).

-spec d_disj_uprohib(U, ARset, ATIset, ATEset) -> Result when
      U :: pm:u(),
      ARset :: pm:id(),
      ATIset :: pm:id(),
      ATEset :: pm:id(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete a prohibition
d_disj_uprohib(#u{id = U_id}, ARset, ATIset, ATEset) ->
    gen_server:call(?SERVER, {d_prohib, U_id, ARset, ATIset, ATEset, u_deny_disj}).

-spec d_disj_pprohib(P, ARset, ATIset, ATEset) -> Result when
      P :: pid(),
      ARset :: pm:id(),
      ATIset :: pm:id(),
      ATEset :: pm:id(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete a prohibition
%% TODO: isn't the is_pid(P) a little to defensive?
d_disj_pprohib(P, ARset, ATIset, ATEset) when is_pid(P) ->
    gen_server:call(?SERVER, {d_prohib, P, ARset, ATIset, ATEset, p_deny_disj}).

-spec d_disj_uaprohib(UA, ARset, ATIset, ATEset) -> Result when
      UA :: pm:ua(),
      ARset :: pm:id(),
      ATIset :: pm:id(),
      ATEset :: pm:id(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete a prohibition
d_disj_uaprohib(#ua{id = UA_id}, ARset, ATIset, ATEset) ->
    gen_server:call(?SERVER, {d_prohib, UA_id, ARset, ATIset, ATEset, ua_deny_disj}).

-spec d_oblig(U, Pattern, Response) -> Result when
      U :: pm:u(),
      Pattern :: pm:id(),
      Response :: pm:id(),
      Result :: ok | {error, Reason :: term()}.
%% @doc delete obligation
d_oblig(#u{id = U_id}, Pattern, Response) ->
    gen_server:call(?SERVER, {d_oblig, U_id, Pattern, Response}).

-spec get_digraph() -> Result when
      Result :: {ok, digraph:graph()} | {error, Reason :: term()}.
%% Return the handle of the digraph
get_digraph() ->
    gen_server:call(?SERVER, get_digraph).

-spec register_p(P, U) -> Result when
      P :: pid(),
      U :: pm:u(),
      Result :: ok | {error, already_exists} | {error, notfound}.
%% @doc Register process for the user.
register_p(P, #u{id = Id}) when is_pid(P) ->
    gen_server:call(?SERVER, {register_p, P, Id});
register_p(P, U) ->
    {error, {badarg, {P, U}}}.

-spec unregister_p(P) -> Result when
      P :: pid(),
      Result :: no_return().
%% @doc Unregister process
unregister_p(P) ->
    gen_server:cast(?SERVER, {unregister_p, P}).

-spec users(UA) -> [pm:id()] when
      UA :: pm:ua().
%% @doc The `users' function represents the mapping from a user
%% attribute to the set of users that are contained by that user
%% attribute `UA'. The function returns a list with the ids of the
%% users.
users(UA) ->
    gen_server:call(?SERVER, {users, UA}).

-spec objects(OA) -> [pm:id()] when
      OA :: pm:oa().
%% @doc The `objects' function represents the mapping from a object
%% attribute to the set of objects that are contained by that object
%% attribute `OA'. The function returns a list with the ids of the
%% objects.
objects(OA) ->
    gen_server:call(?SERVER, {objects, OA}).

-spec elements(PE) -> [pm:id()] when
      PE :: pm:pe().
%% @doc The `elements' function represents the mapping from a given
%% policy element `PE' to the set of policy elements that includes the
%% policy element and all the policy elements contained by that policy
%% element. The function returns a list with the ids of the
%% policy elements.
elements(PE) ->
    gen_server:call(?SERVER, {elements, PE}).

-spec icap(UA) -> [{ARset, AT}] when
      UA :: pm:ua(),
      ARset :: pm:id(),
      AT :: pm:id().
%% @doc The `icap' function returns the Inherent Capabilities of a
%% User Attribute `UA'.
icap(UA) ->
    gen_server:call(?SERVER, {icap, UA}).

-spec iae(AT) -> [{UA, ARset}] when
      AT :: pm:ua() | pm:o() | pm:oa(),
      ARset :: pm:id(),
      UA :: pm:ua().
%% @doc The `iae' function returns the Inherent Aeabilities of a
%% User Attribute `UA'.
iae(AT) ->
    gen_server:call(?SERVER, {iae, AT}).

-spec disj_range(ATIs, ATEs) -> [AT] when
      ATIs :: [AT],
      ATEs :: [AT],
      AT :: pm:id().
%% @doc The disjunctive range `disj_range' function represents the
%% mapping from two constraint sets of attributes, the first `ATIs'
%% designating policy elements for inclusion, and the second `ATEs'
%% designating policy elements for exclusion, to a set of policy
%% elements formed by logical disjunction of the policy elements
%% contained within or not contained respectively within the subgraphs
%% of the referent attributes of each constraint set.
disj_range(ATIs, ATEs) ->
    gen_server:call(?SERVER, {disj_range, ATIs, ATEs}).

-spec conj_range(ATIs, ATEs) -> [AT] when
      ATIs :: [AT],
      ATEs :: [AT],
      AT :: pm:id().
%% @doc The conjunctive range `conj_range' function represents the
%% mapping from two constraint sets of attributes, the first `ATIs'
%% designating policy elements for inclusion, and the second `ATEs'
%% designating policy elements for exclusion, to a set of policy
%% elements formed by logical conjunction of the policy elements
%% contained by or not contained by the attributes of each constraint
%% set respectively.
conj_range(ATIs, ATEs) ->
    gen_server:call(?SERVER, {conj_range, ATIs, ATEs}).

rebuild() -> 
    gen_server:call(?SERVER, rebuild).
   

%% @doc Start server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop server
stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    G = digraph:new([acyclic, protected]),
    pm_pip:rebuild(G),
    PU = [],
    {ok, #state{g = G, pu = PU}}.

%% @private
%% handle_call(rebuild, _From, #state{g = G} = State) ->
%%     digraph:delete(G),
%%     G1 = digraph:new([acyclic, protected]),
%%     Reply = pm_pip:rebuild(G1),
%%     {reply, Reply, State#state{g = G1}};
handle_call({c_u_in_ua, U, UA}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				Id = pm_pip:allocate_id(u),
				pm_pip:create_u_in_ua(G, U#u{id = Id}, UA)
 			end),
    {reply, Reply, State};
handle_call({c_ua_in_ua, UA1, UA2}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				Id = pm_pip:allocate_id(ua),
				pm_pip:create_ua_in_ua(G, UA1#ua{id = Id}, UA2)
			end),
    {reply, Reply, State};
handle_call({c_ua_in_pc, UA, PC}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				Id = pm_pip:allocate_id(ua),
				pm_pip:create_ua_in_pc(G, UA#ua{id = Id}, PC)
			end),
    {reply, Reply, State};
handle_call({c_o_in_oa, O, OA}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				Id = pm_pip:allocate_id(o),
				pm_pip:create_o_in_oa(G, O#o{id = Id}, OA)
			end),
    {reply, Reply, State};
handle_call({c_oa_in_oa, OA1, OA2}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				Id = pm_pip:allocate_id(oa),
				pm_pip:create_oa_in_oa(G, OA1#oa{id = Id}, OA2)
			end),
    {reply, Reply, State};
handle_call({c_oa_in_pc, OA, PC}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				Id = pm_pip:allocate_id(oa),
				pm_pip:create_oa_in_pc(G, OA#oa{id = Id}, PC)
			end),
    {reply, Reply, State};
handle_call({c_assign, X, Y}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				pm_pip:create_assign(G, X, Y)
			end),
    {reply, Reply, State};
handle_call({c_pc, PC}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				Id = pm_pip:allocate_id(pc),
				pm_pip:create_pc(G, PC#pc{id = Id})
			end),
    {reply, Reply, State};
handle_call({c_assoc, X, AR_ids, Z}, _From, State) ->
    Reply = transaction(fun() ->
				ARset = pm_pip:allocate_id(arset, AR_ids),
				pm_pip:create_arset(ARset, AR_ids),
				pm_pip:create_assoc(X, ARset, Z),
				{ok, ARset}
			end),
    {reply, Reply, State};
handle_call({c_prohib, P, AR_ids, ATI_ids, ATE_ids, Prohib_table}, _From, #state{pu = PU} = State)
  when is_pid(P) ->
    W = pm_pip:process_user(P, PU),
    handle_call({c_prohib, W, AR_ids, ATI_ids, ATE_ids, Prohib_table}, _From, State);
handle_call({c_prohib, W, AR_ids, ATI_ids, ATE_ids, Prohib_table}, _From, State) ->
    %% TODO: No checks on defining the same prohibition over and over again.
    Reply = transaction(fun() ->
				ARset = pm_pip:allocate_id(arset, AR_ids),
				pm_pip:create_arset(ARset, AR_ids),
				ATIset = pm_pip:allocate_id(atset, ATI_ids),
				pm_pip:create_atiset(ATIset, ATI_ids),
				ATEset = pm_pip:allocate_id(atset, ATE_ids),
				pm_pip:create_ateset(ATEset, ATE_ids),
				pm_pip:create_prohib(W, ARset, ATIset, ATEset, Prohib_table),
				{ok, {ARset, ATIset, ATEset}}
			end),
    {reply, Reply, State};
handle_call({eval_pattern, _P, _Patterns}, _From, State) ->
    Reply = {error, not_implemented},
    {reply, Reply, State};
handle_call({eval_response, _P, _Responses}, _From, State) ->
    Reply = {error, not_implemented},
    {reply, Reply, State};
handle_call({c_oblig, P, Patterns, Responses}, _From, #state{pu = PU} = State) ->
    Reply = transaction(fun() ->
				U = pm_pip:process_user(P, PU),
				Pattern_id = pm_pip:allocate_id(pattern),
				pm_pip:create_pattern(Pattern_id, Patterns),
				Response_id = pm_pip:allocate_id(response),
				pm_pip:create_response(Response_id, Responses),
				pm_pip:create_oblig(U, Pattern_id, Response_id),
				{ok, {U, Pattern_id, Response_id}}
			end),
    {reply, Reply, State};
%% TODO: Review the test for record existing before deletion. Deleting
%% in Mnesia always yield an 'ok' even if the record didn't exist.
handle_call({d_u_in_ua, X, Y}, _From, #state{g = G, pu = PU} = State) ->
    Reply = transaction(fun() ->
				false = lists:keymember(X, #process_user.u, PU),
				pm_pip:delete_assign(G, X, Y),
				pm_pip:delete_u(G, X)
			end),
    {reply, Reply, State};
handle_call({d_ua_in_y, X, Y}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				pm_pip:delete_assign(G, X, Y),
				pm_pip:delete_ua(G, X)
			end),
    {reply, Reply, State};
handle_call({d_o_in_oa, X, Y}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				pm_pip:delete_assign(G, X, Y),
				pm_pip:delete_o(G, X)
			end),
    {reply, Reply, State};
handle_call({d_oa_in_y, X, Y}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				pm_pip:delete_assign(G, X, Y),
				pm_pip:delete_oa(G, X)
			end),
    {reply, Reply, State};
handle_call({d_assign, X, Y}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				pm_pip:delete_assign(G, X, Y)
			end),
    {reply, Reply, State};
handle_call({d_pc, X}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				pm_pip:delete_pc(G, X)
			end),
    {reply, Reply, State};
handle_call({d_assoc, UA, ARset, AT}, _From, State) ->
    Reply = transaction(fun() ->
				pm_pip:delete_assoc(UA, ARset, AT),
				pm_pip:delete_arset(ARset)
			end),
    {reply, Reply, State};
handle_call({d_prohib, P, ARset, ATIset, ATEset, Prohib_table}, _From, #state{pu = PU} = State)
  when is_pid(P) ->
    W = pm_pip:process_user(P, PU),
    handle_call({d_prohib, W, ARset, ATIset, ATEset, Prohib_table}, _From, State);
handle_call({d_prohib, W, ARset, ATIset, ATEset, Prohib_table}, _From, State) ->
    Reply = transaction(fun() ->
				pm_pip:delete_prohib(W, ARset, ATIset, ATEset, Prohib_table),
				pm_pip:delete_arset(ARset),
				pm_pip:delete_atiset(ATIset),
				pm_pip:delete_ateset(ATEset)
			end),
    {reply, Reply, State};
handle_call({d_oblig, U, Pattern_id, Response_id}, _From, State) ->
    Reply = transaction(fun() ->
				pm_pip:delete_oblig(U, Pattern_id, Response_id),
				pm_pip:delete_pattern(Pattern_id),
				pm_pip:delete_response(Response_id)
			end),
    {reply, Reply, State};
handle_call(get_digraph, _From, #state{g = G} = State) ->
    Reply = {ok, G},
    {reply, Reply, State};
handle_call({register_p, P, U}, _From, #state{pu = PU} = State) ->
    case mnesia:dirty_read(u, U) of
	[#u{}] ->
	    case lists:keymember(P, #process_user.p, PU) of
		false ->
		    link(P),
		    {reply, ok, State#state{pu = [#process_user{p = P, u = U} | PU]}};
		true ->
		    {reply, {error, already_exists}, State}
	    end;
	[] ->
	    {reply, {error, not_found}, State}
    end;
handle_call({users, UA}, _From, #state{g = G} = State) ->
    Reply = pm_pip:users(G, UA),
    {reply, Reply, State};
handle_call({objects, OA}, _From, #state{g = G} = State) ->
    Reply = pm_pip:objects(G, OA),
    {reply, Reply, State};
handle_call({elements, PE}, _From, #state{g = G} = State) ->
    Reply = pm_pip:elements(G, PE),
    {reply, Reply, State};
%% TODO: the icap and iae functions currently use a transaction, but
%% this is not needed if the pap is the only process to access the
%% PIP. If however the PIP is distributed with more than one pm_pip,
%% and Mnesia has to deal with concurrency, the transaction _is_
%% needed. (The icap and iae functions don't make changes to the
%% database and therefor could be implemented 'dirty'.)
handle_call({icap, UA}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				pm_pip:icap(G, UA)
			end),
    {reply, Reply, State};
handle_call({iae, AT}, _From, #state{g = G} = State) ->
    Reply = transaction(fun() ->
				pm_pip:iae(G, AT)
			end),
    {reply, Reply, State};
handle_call({disj_range, ATIs, ATEs}, _From, #state{g = G} = State) ->
    Reply = pm_pip:disj_range(G, ATIs, ATEs),
    {reply, Reply, State};
handle_call({conj_range, ATIs, ATEs}, _From, #state{g = G} = State) ->
    Reply = pm_pip:conj_range(G, ATIs, ATEs),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    Reply = {error, {not_implemented, Request}},
    {reply, Reply, State}.

%% @private
handle_cast({unregister_p, P}, #state{pu = PU1} = State) ->
    case lists:keytake(P, #process_user.p, PU1) of
	{value, #process_user{p = P}, PU2} ->
	    unlink(P),
	    {noreply, State#state{pu = PU2}};
	false ->
	    {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @private
handle_info({'EXIT', Pid, _Reason}, #state{pu = PU1} = State) ->
    PU2 = lists:keydelete(Pid, #process_user.p, PU1),
    {noreply, State#state{pu = PU2}};
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

%% @private @doc Most PAP calls are transactions which make changes to
%% the database. If something goes wrong, all changes to the database
%% should be rolled back.

%% TODO: The Mnesia database is rolled back but changes to the digraph
%% are not which leaves the digraph in a non-consistent state. Either
%% the server should crash, taking the digraph with it, or we should
%% think about a rollback mechanism for the digraph.
transaction(Fun) ->
    pm_pip:transaction(Fun).

%% %%%===================================================================
%% %%% Tests
%% %%%===================================================================

-ifdef(EUNIT).

server_test_() ->
    {setup,
     fun server_setup/0,
     fun server_cleanup/1,
     fun tsts/1}.

disj_range_test_() ->
    {setup,
     fun server_setup/0,
     fun server_cleanup/1,
     fun disj_conj_range_tst/1}.

server_setup() ->
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
    Dir.

server_cleanup(Dir) ->
    pm_pap:stop(),
    application:stop(mnesia), % stop mnesia
    {ok, Files} = file:list_dir(Dir),
    [file:delete(filename:join([Dir, File])) || File <- Files],
    ok = file:del_dir(Dir),
    ok.

%% TODO: add more tests. Some tests not yet implemented
tsts(_Pids) ->
    [tst_c_pc(),
     tst_c_ua_in_pc(),
     tst_c_u_in_ua(),
     tst_c_ua_in_ua(),
     tst_c_u_to_ua(),
     tst_c_ua_to_ua(),
     tst_c_ua_to_pc(),
     tst_c_oa_in_pc(),
     tst_c_o_in_oa(),
     tst_c_oa_in_oa(),
     tst_c_o_to_oa(),
     tst_c_oa_to_oa(),
     tst_c_oa_to_pc(),
     tst_c_assoc(),
     tst_c_oblig(),
     tst_c_conj_uprohib(),
     tst_c_conj_pprohib(),
     tst_c_conj_uaprohib(),
     tst_c_disj_uprohib(),
     tst_c_disj_pprohib(),
     tst_c_disj_uaprohib(),
     %%      tst_eval_pattern(),
     %%      tst_eval_response()
     tst_d_u_in_ua(),
     tst_d_u_to_ua(),
     tst_d_ua_in_ua(),
     tst_d_ua_to_ua(),
     tst_d_ua_in_pc(),
     tst_d_ua_to_pc(),
     tst_d_o_in_oa(),
     tst_d_o_to_oa(),
     tst_d_oa_in_oa(),
     tst_d_oa_to_oa(),
     tst_d_oa_in_pc(),
     tst_d_oa_to_pc(),
     tst_d_pc(),
     tst_d_assoc(),
     tst_d_oblig(),
     tst_d_conj_uprohib(),
     tst_d_conj_pprohib(),
     tst_d_conj_uaprohib(),
     tst_d_disj_uprohib(),
     tst_d_disj_pprohib(),
     tst_d_disj_uaprohib(),
     tst_users_objects_elements(),
     tst_icap_iae()
    ].

%% NB: Testing the disj_range and conj_range functions requires the
%% unit test to be ran without other tests. The issue is that the
%% digraph must only contain the elements added during the test and
%% not from the other tests as well. Processing the `tsts' list and
%% executing the actual tests are two distinct steps with the first
%% making changes to the digraph which will mess-up the tests.
disj_conj_range_tst(_Pids) ->
    [tst_disj_conj_range()].


tst_c_pc() ->
    [?_assertMatch({ok, #pc{}}, pm_pap:c_pc(#pc{value = "My PC"}))].

tst_c_ua_in_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    [?_assertMatch({ok, #ua{}}, pm_pap:c_ua_in_pc(#ua{value = "My ua"}, PC))].

tst_c_u_in_ua() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "a ua"}, PC),
    [?_assertMatch({ok, #u{}}, pm_pap:c_u_in_ua(#u{value = "a user"}, UA))].

tst_c_ua_in_ua() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    [?_assertMatch({ok, #ua{}}, pm_pap:c_ua_in_ua(#ua{value = "another attribute"}, UA))].

tst_c_u_to_ua() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_pc(#ua{value = "another attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA1),
    [?_assertMatch(ok, pm_pap:c_u_to_ua(U, UA2))].

tst_c_ua_to_ua() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_pc(#ua{value = "another attribute"}, PC),
    [?_assertMatch(ok, pm_pap:c_ua_to_ua(UA1, UA2))].

tst_c_ua_to_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_ua(#ua{value = "another attribute"}, UA1),
    [?_assertMatch(ok, pm_pap:c_ua_to_pc(UA2, PC))].

tst_c_oa_in_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    [?_assertMatch({ok, #oa{}}, pm_pap:c_oa_in_pc(#oa{value = "My oa"}, PC))].

tst_c_o_in_oa() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "a oa"}, PC),
    [?_assertMatch({ok, #o{}}, pm_pap:c_o_in_oa(#o{value = "a usetr"}, OA))].

tst_c_oa_in_oa() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribote"}, PC),
    [?_assertMatch({ok, #oa{}}, pm_pap:c_oa_in_oa(#oa{value = "another attribote"}, OA))].

tst_c_o_to_oa() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribote"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "another attribote"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "a usetr"}, OA1),
    [?_assertMatch(ok, pm_pap:c_o_to_oa(O, OA2))].

tst_c_oa_to_oa() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribote"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "another attribote"}, PC),
    [?_assertMatch(ok, pm_pap:c_oa_to_oa(OA1, OA2))].

tst_c_oa_to_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribote"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_oa(#oa{value = "another attribute"}, OA1),
    [?_assertMatch(ok, pm_pap:c_oa_to_pc(OA2, PC))].

tst_c_assoc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    [?_assertMatch({ok, _}, pm_pap:c_assoc(UA, [#ar{id = 'r'}], OA)),
     ?_assertMatch({ok, _}, pm_pap:c_assoc(UA, [#ar{id = 'r'}], OA)), % TODO: bad?
     ?_assertMatch({ok, _}, pm_pap:c_assoc(UA, [#ar{id = 'w'}], O)),
     ?_assertMatch({error, _Reason}, pm_pap:c_assoc(U, [#ar{id = 'r'}], OA)),
     ?_assertMatch({error, _Reason}, pm_pap:c_assoc(UA, [#ar{id = 'x'}], OA))].

tst_c_oblig() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    P = spawn_link(fun() -> receive _ -> ok end end),
    Q = spawn_link(fun() -> receive _ -> ok end end),
    pm_pap:register_p(P, U),
    Patterns = [#pattern{id = Id, value = "blabla"} || Id <- [p1, p2, p3]],
    Responses = [#response{id = Id, value = "blabla"} || Id <- [r1, r2, r3]],
    [?_assertMatch({ok, _}, pm_pap:c_oblig(P, Patterns, Responses)),
     ?_assertMatch({error, _Reason}, pm_pap:c_oblig(Q, Patterns, Responses))].

tst_c_conj_uprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    [
     ?_assertMatch({ok, _}, pm_pap:c_conj_uprohib(U, ARs, [OA1], [OA2])),
     ?_assertMatch({ok, _}, pm_pap:c_conj_uprohib(U, ARs, [O], [])),
     ?_assertMatch({ok, _}, pm_pap:c_conj_uprohib(U, ARs, [OA1], [])),
     ?_assertMatch({ok, _}, pm_pap:c_conj_uprohib(U, ARs, [], [OA2])),
     ?_assertMatch({ok, _}, pm_pap:c_conj_uprohib(U, ARs, [UA1], [UA2])),
     ?_assertMatch({ok, _}, pm_pap:c_conj_uprohib(U, ARs, [UA1], [])),
     ?_assertMatch({ok, _}, pm_pap:c_conj_uprohib(U, ARs, [], [UA2])),
     ?_assertMatch({error, badvalue}, pm_pap:c_conj_uprohib(U, [], [OA1], [OA2])),
     ?_assertMatch({error, badvalue}, pm_pap:c_conj_uprohib(U, ARs, [], [])),
     ?_assertMatch({error, {badarg, _}}, pm_pap:c_conj_uprohib(UA, ARs, [OA1], [OA2])),
     ?_assertError(function_clause, pm_pap:c_conj_uprohib(U, ARs, [OA1], [UA2])),
     ?_assertError(function_clause, pm_pap:c_conj_uprohib(U, ARs, [UA1, OA1], [UA2])),
     ?_assertMatch({error, _}, pm_pap:c_conj_uprohib(U, [#ar{id = 'x'}], [UA1], [UA2])),
     ?_assertMatch({error, _}, pm_pap:c_conj_uprohib(U, ARs, [#ua{id = abc}], [UA2])),
     ?_assertMatch({error, _}, pm_pap:c_conj_uprohib(U, ARs, [UA1], [#ua{id = bcd}]))    ].

tst_c_conj_pprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    P = spawn_link(fun() -> receive _ -> ok end end),
    pm_pap:register_p(P, U),
    Q = spawn_link(fun() -> receive _ -> ok end end), % Process, unlinked to a U
    [?_assertMatch({ok, _}, pm_pap:c_conj_pprohib(P, ARs, [OA1, OA2], [O])),
     ?_assertMatch({error, {badarg, _}}, pm_pap:c_conj_pprohib(U, ARs, [OA1, OA2], [O])),
     ?_assertMatch({error, {badarg, _}}, pm_pap:c_conj_pprohib(UA, ARs, [OA1, OA2], [O])),
     %% A process, but not a registered one.
     ?_assertMatch({error, _}, pm_pap:c_conj_pprohib(Q, ARs, [OA1, OA2], [O]))
    ].

tst_c_conj_uaprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    ARs = [#ar{id = 'w'}],
    [?_assertMatch({ok, _}, pm_pap:c_conj_uaprohib(UA, ARs, [OA1], [OA2])),
     ?_assertMatch({error, {badarg, _}}, pm_pap:c_conj_uaprohib(U, ARs, [OA1], [OA2]))
    ].

tst_c_disj_uprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    [
     ?_assertMatch({ok, _}, pm_pap:c_disj_uprohib(U, ARs, [OA1], [OA2])),
     ?_assertMatch({ok, _}, pm_pap:c_disj_uprohib(U, ARs, [O], [])),
     ?_assertMatch({ok, _}, pm_pap:c_disj_uprohib(U, ARs, [OA1], [])),
     ?_assertMatch({ok, _}, pm_pap:c_disj_uprohib(U, ARs, [], [OA2])),
     ?_assertMatch({ok, _}, pm_pap:c_disj_uprohib(U, ARs, [UA1], [UA2])),
     ?_assertMatch({ok, _}, pm_pap:c_disj_uprohib(U, ARs, [UA1], [])),
     ?_assertMatch({ok, _}, pm_pap:c_disj_uprohib(U, ARs, [], [UA2])),
     ?_assertMatch({error, badvalue}, pm_pap:c_disj_uprohib(U, [], [OA1], [OA2])),
     ?_assertMatch({error, badvalue}, pm_pap:c_disj_uprohib(U, ARs, [], [])),
     ?_assertMatch({error, {badarg, _}}, pm_pap:c_disj_uprohib(UA, ARs, [OA1], [OA2])),
     ?_assertError(function_clause, pm_pap:c_disj_uprohib(U, ARs, [OA1], [UA2])),
     ?_assertError(function_clause, pm_pap:c_disj_uprohib(U, ARs, [UA1, OA1], [UA2])),
     ?_assertMatch({error, _}, pm_pap:c_disj_uprohib(U, [#ar{id = 'x'}], [UA1], [UA2])),
     ?_assertMatch({error, _}, pm_pap:c_disj_uprohib(U, ARs, [#ua{id = abc}], [UA2])),
     ?_assertMatch({error, _}, pm_pap:c_disj_uprohib(U, ARs, [UA1], [#ua{id = bcd}]))    ].

tst_c_disj_pprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    P = spawn_link(fun() -> receive _ -> ok end end),
    pm_pap:register_p(P, U),
    Q = spawn_link(fun() -> receive _ -> ok end end), % Process, unlinked to a U
    [?_assertMatch({ok, _}, pm_pap:c_disj_pprohib(P, ARs, [OA1, OA2], [O])),
     ?_assertMatch({error, {badarg, _}}, pm_pap:c_disj_pprohib(U, ARs, [OA1, OA2], [O])),
     ?_assertMatch({error, {badarg, _}}, pm_pap:c_disj_pprohib(UA, ARs, [OA1, OA2], [O])),
     ?_assertMatch({error, _}, pm_pap:c_disj_pprohib(Q, ARs, [OA1, OA2], [O]))
    ].

tst_c_disj_uaprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    ARs = [#ar{id = 'w'}],
    [?_assertMatch({ok, _}, pm_pap:c_disj_uaprohib(UA, ARs, [OA1], [OA2])),
     ?_assertMatch({error, {badarg, _}}, pm_pap:c_disj_uaprohib(U, ARs, [OA1], [OA2]))
    ].

%% tst_eval_pattern() ->
%%     ?debugMsg("eval_pattern: Not yet implemented"),
%%     {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
%%     {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
%%     {ok, U} = pm_pap:c_u_in_ua(UA),
%%     P = spawn_link(fun() -> receive _ -> ok end end),
%%     ok = pm_pip:create_p(P, U#user.id),
%%     Pattern = [p1, p2, p3],
%%     [?_assertMatch({error, not_implemented}, pm_pap:eval_pattern(P, Pattern))].

%% tst_eval_response() ->
%%     ?debugMsg("eval_response: Not yet implemented"),
%%     {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}) ,
%%     {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
%%     {ok, U} = pm_pap:c_u_in_ua(UA),
%%     P = spawn_link(fun() -> receive _ -> ok end end),
%%     ok = pm_pip:create_p(P, U#user.id),
%%     Response = [p1, p2, p3],
%%     [?_assertMatch({error, not_implemented}, pm_pap:eval_response(P, Response))].

tst_d_u_in_ua() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "another attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    [?_assertMatch(ok, pm_pap:d_u_in_ua(U, UA))].

tst_d_u_to_ua() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "another attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_pc(#ua{value = "another attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA1),
    ok = pm_pap:c_u_to_ua(U, UA2),
    [?_assertMatch(ok, pm_pap:d_u_to_ua(U, UA2))].

tst_d_ua_in_ua() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_ua(#ua{value = "another attribute"}, UA1),
    [?_assertMatch(ok, pm_pap:d_ua_in_ua(UA2, UA1))].

tst_d_ua_to_ua() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "another attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_pc(#ua{value = "another attribute"}, PC),
    ok = pm_pap:c_ua_to_ua(UA1, UA2),
    [?_assertMatch(ok, pm_pap:d_ua_to_ua(UA1, UA2))].

tst_d_ua_in_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    [?_assertMatch(ok, pm_pap:d_ua_in_pc(UA, PC))].

tst_d_ua_to_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "another attribute"}, PC),
    {ok, UA2} = pm_pap:c_ua_in_ua(#ua{value = "another attribute"}, UA1),
    ok = pm_pap:c_ua_to_pc(UA2, PC),
    [?_assertMatch(ok, pm_pap:d_ua_to_pc(UA2, PC))].

tst_d_o_in_oa() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "another attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "a user"}, OA),
    [?_assertMatch(ok, pm_pap:d_o_in_oa(O, OA))].

tst_d_o_to_oa() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "another attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "another attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "a user"}, OA1),
    ok = pm_pap:c_o_to_oa(O, OA2),
    [?_assertMatch(ok, pm_pap:d_o_to_oa(O, OA2))].

tst_d_oa_in_oa() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_oa(#oa{value = "another attribute"}, OA1),
    [?_assertMatch(ok, pm_pap:d_oa_in_oa(OA2, OA1))].

tst_d_oa_to_oa() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "another attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "another attribute"}, PC),
    ok = pm_pap:c_oa_to_oa(OA1, OA2),
    [?_assertMatch(ok, pm_pap:d_oa_to_oa(OA1, OA2))].

tst_d_oa_in_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    [?_assertMatch(ok, pm_pap:d_oa_in_pc(OA, PC))].

tst_d_oa_to_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "another attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_oa(#oa{value = "another attribute"}, OA1),
    ok = pm_pap:c_oa_to_pc(OA2, PC),
    [?_assertMatch(ok, pm_pap:d_oa_to_pc(OA2, PC))].

tst_d_pc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a PC"}),
    [?_assertMatch(ok, pm_pap:d_pc(PC))].

tst_d_assoc() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, UA1} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    {ok, Association1} = pm_pap:c_assoc(UA, [#ar{id = 'r'}, #ar{id = 'w'}], OA),
    {ok, Association2} = pm_pap:c_assoc(UA, [#ar{id = 'r'}, #ar{id = 'w'}], O),
    {ok, Association3} = pm_pap:c_assoc(UA, [#ar{id = 'r'}, #ar{id = 'w'}], UA1),
    ARset1 = Association1#association.arset,
    ARset2 = Association2#association.arset,
    ARset3 = Association3#association.arset,
    [?_assertMatch(ok, pm_pap:d_assoc(UA, ARset1, OA)),
     ?_assertMatch(ok, pm_pap:d_assoc(UA, ARset2, O)),
     ?_assertMatch(ok, pm_pap:d_assoc(UA, ARset3, UA1)),
     ?_assertMatch({error, _}, pm_pap:d_assoc(UA, ARset3, UA1))]. % ARset doesn't exist anymore

tst_d_oblig() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    P = spawn_link(fun() -> receive _ -> ok end end),
    pm_pap:register_p(P, U),
    Patterns = [#pattern{id = Id, value = "blabla"} || Id <- [p1, p2, p3]],
    Responses = [#response{id = Id, value = "blabla"} || Id <- [r1, r2, r3]],
    {ok, #obligation{pattern = Pattern, response = Response} = _Oblig} = pm_pap:c_oblig(P, Patterns, Responses),
    [?_assertMatch(ok, pm_pap:d_oblig(U, Pattern, Response))].

tst_d_conj_uprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    {ok, U_deny_conj} = pm_pap:c_conj_uprohib(U, ARs, [OA1, OA2], [O]),
    ARset = U_deny_conj#u_deny_conj.arset,
    ATIset = U_deny_conj#u_deny_conj.atiset,
    ATEset = U_deny_conj#u_deny_conj.ateset,
    [?_assertMatch(ok, pm_pap:d_conj_uprohib(U, ARset, ATIset, ATEset))].

tst_d_conj_pprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    P = spawn_link(fun() -> receive _ -> ok end end),
    pm_pap:register_p(P, U),
    {ok, P_deny_conj} = pm_pap:c_conj_pprohib(P, ARs, [OA1, OA2], [O]),
    ARset = P_deny_conj#p_deny_conj.arset,
    ATIset = P_deny_conj#p_deny_conj.atiset,
    ATEset = P_deny_conj#p_deny_conj.ateset,
    [?_assertMatch(ok, pm_pap:d_conj_pprohib(P, ARset, ATIset, ATEset))].

tst_d_conj_uaprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    {ok, UA_deny_conj} = pm_pap:c_conj_uaprohib(UA, ARs, [OA1, OA2], [O]),
    ARset = UA_deny_conj#ua_deny_conj.arset,
    ATIset = UA_deny_conj#ua_deny_conj.atiset,
    ATEset = UA_deny_conj#ua_deny_conj.ateset,
    [?_assertMatch(ok, pm_pap:d_conj_uaprohib(UA, ARset, ATIset, ATEset))].

tst_d_disj_uprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    {ok, U_deny_disj} = pm_pap:c_disj_uprohib(U, ARs, [OA1, OA2], [O]),
    ARset = U_deny_disj#u_deny_disj.arset,
    ATIset = U_deny_disj#u_deny_disj.atiset,
    ATEset = U_deny_disj#u_deny_disj.ateset,
    [?_assertMatch(ok, pm_pap:d_disj_uprohib(U, ARset, ATIset, ATEset))].

tst_d_disj_pprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, U} = pm_pap:c_u_in_ua(#u{value = "a user"}, UA),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    P = spawn_link(fun() -> receive _ -> ok end end),
    pm_pap:register_p(P, U),
    {ok, P_deny_disj} = pm_pap:c_disj_pprohib(P, ARs, [OA1, OA2], [O]),
    ARset = P_deny_disj#p_deny_disj.arset,
    ATIset = P_deny_disj#p_deny_disj.atiset,
    ATEset = P_deny_disj#p_deny_disj.ateset,
    [?_assertMatch(ok, pm_pap:d_disj_pprohib(P, ARset, ATIset, ATEset))].

tst_d_disj_uaprohib() ->
    {ok, PC} = pm_pap:c_pc(#pc{value = "a pc"}),
    {ok, UA} = pm_pap:c_ua_in_pc(#ua{value = "an attribute"}, PC),
    {ok, OA} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA1} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, OA2} = pm_pap:c_oa_in_pc(#oa{value = "an attribute"}, PC),
    {ok, O} = pm_pap:c_o_in_oa(#o{value = "an object"}, OA),
    ARs = [#ar{id = 'w'}],
    {ok, UA_deny_disj} = pm_pap:c_disj_uaprohib(UA, ARs, [OA1, OA2], [O]),
    ARset = UA_deny_disj#ua_deny_disj.arset,
    ATIset = UA_deny_disj#ua_deny_disj.atiset,
    ATEset = UA_deny_disj#ua_deny_disj.ateset,
    [?_assertMatch(ok, pm_pap:d_disj_uaprohib(UA, ARset, ATIset, ATEset))].

tst_users_objects_elements() ->
    {ok, PC} = c_pc(#pc{value = "Bank Teller example"}),
    {ok, Branch_1_usr_attr} = c_ua_in_pc(#ua{value = "Branch 1 user attribute"}, PC),
    {ok, Branch_1_obj_attr} = c_oa_in_pc(#oa{value = "Branch 1 object attribute"}, PC),
    {ok, Teller} = c_ua_in_ua(#ua{value = "Teller"}, Branch_1_usr_attr),
    {ok, Auditor} = c_ua_in_ua(#ua{value = "Auditor"}, Branch_1_usr_attr),
    {ok, U1} = c_u_in_ua(#u{value = "User u1"}, Teller),
    {ok, U2} = c_u_in_ua(#u{value = "User u2"}, Auditor),
    {ok, Accounts} = c_oa_in_oa(#oa{value = "accounts"}, Branch_1_obj_attr),
    {ok, O1} = c_o_in_oa(#o{value = "Account o1"}, Accounts),
    [?_assertEqual(lists:sort([U1#u.id, U2#u.id]), lists:sort(users(Branch_1_usr_attr))),
     ?_assertEqual([O1#o.id], objects(Branch_1_obj_attr)),
     ?_assertEqual(lists:sort([PC#pc.id, 
			       Branch_1_usr_attr#ua.id, Branch_1_obj_attr#oa.id,
			       Teller#ua.id, Auditor#ua.id, U1#u.id, U2#u.id,
			       Accounts#oa.id, O1#o.id]),
		   lists:sort(elements(PC)))].

tst_icap_iae() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, UA1} =  pm_pap:c_ua_in_pc(#ua{value="ua1"}, PC),
    {ok, UA2} =  pm_pap:c_ua_in_ua(#ua{value="ua2"}, UA1),
    {ok, UA3} =  pm_pap:c_ua_in_ua(#ua{value="ua3"}, UA1),
    {ok, U1} =  pm_pap:c_u_in_ua(#u{value="u1"}, UA2),
    ok = pm_pap:c_u_to_ua(U1, UA3),
    {ok, _U2} =  pm_pap:c_u_in_ua(#u{value="u2"}, UA3),
    {ok, OA1} =  pm_pap:c_oa_in_pc(#oa{value="oa21"}, PC),
    {ok, OA2} =  pm_pap:c_oa_in_oa(#oa{value="oa20"}, OA1),
    {ok, O1} =  pm_pap:c_o_in_oa(#o{value="o1"}, OA2),
    {ok, O2} =  pm_pap:c_o_in_oa(#o{value="o2"}, OA2),
    {ok, Assoc1} = pm_pap:c_assoc(UA1, [#ar{id = 'r'}], OA1),
    {ok, Assoc2} = pm_pap:c_assoc(UA2, [#ar{id = 'w'}], O1),
    {ok, Assoc3} = pm_pap:c_assoc(UA3, [#ar{id = 'w'}], O2),
    ARset1 = Assoc1#association.arset,
    ARset2 = Assoc2#association.arset,
    ARset3 = Assoc3#association.arset,
    [?_assertEqual([{ARset2, O1#o.id}], icap(UA2)),
     ?_assertEqual([{ARset3, O2#o.id}], icap(UA3)),
     ?_assertEqual(lists:sort([{ARset1, OA1#oa.id}, {ARset2, O1#o.id}, {ARset3, O2#o.id}]),
		   lists:sort(icap(UA1))),
     ?_assertEqual([{UA1#ua.id, ARset1}], iae(OA1)),
     ?_assertEqual([{UA1#ua.id, ARset1}], iae(OA2)),
     ?_assertEqual(lists:sort([{UA1#ua.id, ARset1}, {UA2#ua.id, ARset2}]),
		   lists:sort(iae(O1))),
     ?_assertEqual(lists:sort([{UA1#ua.id, ARset1}, {UA3#ua.id, ARset3}]),
		   lists:sort(iae(O2)))].

tst_disj_conj_range() ->
    {ok, PC} = pm_pap:c_pc(#pc{}),
    {ok, UA1} =  pm_pap:c_ua_in_pc(#ua{value="ua1"}, PC),
    {ok, UA2} =  pm_pap:c_ua_in_ua(#ua{value="ua2"}, UA1),
    {ok, UA3} =  pm_pap:c_ua_in_ua(#ua{value="ua3"}, UA1),
    {ok, U1} =  pm_pap:c_u_in_ua(#u{value="u1"}, UA2),
    ok = pm_pap:c_u_to_ua(U1, UA3),
    {ok, U2} =  pm_pap:c_u_in_ua(#u{value="u2"}, UA3),
    {ok, OA1} =  pm_pap:c_oa_in_pc(#oa{value="oa21"}, PC),
    {ok, OA2} =  pm_pap:c_oa_in_oa(#oa{value="oa20"}, OA1),
    {ok, O1} =  pm_pap:c_o_in_oa(#o{value="o1"}, OA2),
    {ok, O2} =  pm_pap:c_o_in_oa(#o{value="o2"}, OA2),
    [?_assertEqual([], disj_range([], [])),
     ?_assertEqual([U1#u.id], disj_range([U1], [])),
     ?_assertEqual(lists:sort([UA1#ua.id, UA2#ua.id, UA3#ua.id, U2#u.id,
			       OA1#oa.id, OA2#oa.id, O1#o.id, O2#o.id]),
		   lists:sort(disj_range([], [U1]))),
     ?_assertEqual(lists:sort([U1#u.id,
			       OA1#oa.id, OA2#oa.id, O1#o.id, O2#o.id]),
		   lists:sort(disj_range([U1], [UA1]))),
     ?_assertEqual(lists:sort([U1#u.id, U2#u.id,
			       OA1#oa.id, OA2#oa.id, O1#o.id, O2#o.id]),
		   lists:sort(disj_range([U1, U2], [UA1]))),
     ?_assertEqual(lists:sort([UA1#ua.id, UA2#ua.id, UA3#ua.id, U1#u.id, U2#u.id,
			       OA1#oa.id, OA2#oa.id, O1#o.id, O2#o.id]),
		   lists:sort(disj_range([U1], [U1]))),
     
     ?_assertEqual([], conj_range([], [])),
     ?_assertEqual([U1#u.id], conj_range([U1], [])),
     ?_assertEqual([], conj_range([], [U1])),
     ?_assertEqual([], conj_range([U1], [UA1])),
     ?_assertEqual([U1#u.id], conj_range([U1], [OA1])),
     ?_assertEqual(lists:sort([UA1#ua.id, UA2#ua.id, UA3#ua.id, U2#u.id]),
		   lists:sort(conj_range([UA1], [U1]))),
     ?_assertEqual([], conj_range([U1], [UA1]))
    ].
    
-endif.
