%%% -*- mode: erlang -*-
-ifndef(pm_hrl).
-define(pm_hrl, true).

%% These records are used for both externally and intenally.
-record(p, {id, value}).
-record(u, {id, value}).
-record(ua, {id, value}).
-record(o, {id, value}).
-record(oa, {id, value}).
-record(pc, {id, value}).

-record(op, {id, value, ref_cnt = 0}).
-record(ar, {id, value, ref_cnt = 0}).

-record(pattern, {id, value}).
-record(response, {id, value}).

-record(association, {ua, arset, at}).
-record(u_deny_conj, {u, arset, atiset, ateset}).
-record(p_deny_conj, {p, arset, atiset, ateset}).
-record(ua_deny_conj, {ua, arset, atiset, ateset}).
-record(u_deny_disj, {u, arset, atiset, ateset}).
-record(p_deny_disj, {p, arset, atiset, ateset}).
-record(ua_deny_disj, {ua, arset, atiset, ateset}).
-record(obligation, {u, pattern, response}).

-record(process_user, {p :: pid(), u}).

%% policy elements, used internally by the PIP.
-record(pe, {id, ref_cnt = 0}).
-record(assign, {a, b, edge}).
-record(prohibition, {a, b, c, d, value}).
-record(set, {id, value, inst_cnt = 0, ref_cnt = 0}).
-record(seq, {id, value, inst_cnt = 0, ref_cnt = 0}).

-endif.
