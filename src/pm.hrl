%%% -*- mode: erlang -*-
-ifndef(pm_hrl).
-define(pm_hrl, true).

%% These records are used for communicating with the PAP
-record(association, {ua, arset, at}).
-record(u_deny_conj, {u, arset, atiset, ateset}).
-record(p_deny_conj, {p, arset, atiset, ateset}).
-record(ua_deny_conj, {ua, arset, atiset, ateset}).
-record(u_deny_disj, {u, arset, atiset, ateset}).
-record(p_deny_disj, {p, arset, atiset, ateset}).
-record(ua_deny_disj, {ua, arset, atiset, ateset}).
-record(obligation, {u, pattern, response}).

%% policy elements, used internaly by the PIP, PAP, PDP etc.
-record(pe, {id, vertex, ref_cnt = 0}).
-record(p, {id, value}).
-record(u, {id, value}).
-record(ua, {id, value}).
-record(o, {id, value}).
-record(oa, {id, value}).
-record(pc, {id, value}).

-record(assign, {a, b, edge}).
-record(assoc, {a, b, c, edge}).
-record(prohib, {a, b, c, d, value}).
-record(oblig, {a, b, c, value}).

-record(op, {id, value, ref_cnt = 0}).
-record(ar, {id, value, ref_cnt = 0}).

-record(set, {id, value, inst_cnt = 0, ref_cnt = 0}).
-record(seq, {id, value, inst_cnt = 0, ref_cnt = 0}).

-record(pattern, {id, value}).
-record(response, {id, value}).

-record(process_user, {p :: pid(), u}).

-endif.
