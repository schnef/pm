%% See http://relops.com/blog/2014/01/13/leex_and_yecc/

Definitions.

%% K   = [A-Za-z0-9_\s\][!\"$%&\'()*+,./:;<>?@\\^_`{}~-]
K   = [A-Za-z0-9_\s\][!\"$%&\'()*+,./:;<=>?@\\^_`{}~-]
WS  = (\||[\000-\037]|#.*)

Rules.

Connector         : {token, {'Connector', TokenLine, list_to_atom(TokenChars)}}.
comps             : {token, {comps, TokenLine, list_to_atom(TokenChars)}}.
conts             : {token, {conts, TokenLine, list_to_atom(TokenChars)}}.
Directory         : {token, {'Directory', TokenLine, list_to_atom(TokenChars)}}.
File              : {token, {'File', TokenLine, list_to_atom(TokenChars)}}.
Object            : {token, {'Object', TokenLine, list_to_atom(TokenChars)}}.
Object\sattribute : {token, {'Object attr', TokenLine, list_to_atom(TokenChars)}}.
Operation\sset    : {token, {'Operation set', TokenLine, list_to_atom(TokenChars)}}.
PM                : {token, {pm, TokenLine, list_to_atom(TokenChars)}}.
Policy\sclass     : {token, {'Policy class', TokenLine, list_to_atom(TokenChars)}}.
User              : {token, {'User', TokenLine, list_to_atom(TokenChars)}}.
User\sattribute   : {token, {'User attr', TokenLine, list_to_atom(TokenChars)}}.
a                 : {token, {a, TokenLine, list_to_atom(TokenChars)}}.
across\ssessions  : {token, {across_sess, TokenLine, list_to_atom(TokenChars)}}.
add               : {token, {add, TokenLine, list_to_atom(TokenChars)}}.
app               : {token, {app, TokenLine, list_to_atom(TokenChars)}}.
as                : {token, {as, TokenLine, list_to_atom(TokenChars)}}.
asg               : {token, {asg, TokenLine, list_to_atom(TokenChars)}}.
b                 : {token, {b, TokenLine, list_to_atom(TokenChars)}}.
c                 : {token, {c, TokenLine, list_to_atom(TokenChars)}}.
cb                : {token, {cb, TokenLine, list_to_atom(TokenChars)}}.
deny              : {token, {deny, TokenLine, list_to_atom(TokenChars)}}.
eml               : {token, {eml, TokenLine, list_to_atom(TokenChars)}}.
fn                : {token, {fn, TokenLine, list_to_atom(TokenChars)}}.
h                 : {token, {h, TokenLine, list_to_atom(TokenChars)}}.
intra\ssession    : {token, {intra_sess, TokenLine, list_to_atom(TokenChars)}}.
key               : {token, {key, TokenLine, list_to_atom(TokenChars)}}.
ks                : {token, {ks, TokenLine, list_to_atom(TokenChars)}}.
no                : {token, {no, TokenLine, list_to_atom(TokenChars)}}.
o                 : {token, {o, TokenLine, list_to_atom(TokenChars)}}.
ob                : {token, {ob, TokenLine, list_to_atom(TokenChars)}}.
oc                : {token, {oc, TokenLine, list_to_atom(TokenChars)}}.
op                : {token, {op, TokenLine, list_to_atom(TokenChars)}}.
p                 : {token, {p, TokenLine, list_to_atom(TokenChars)}}.
prop              : {token, {prop, TokenLine, list_to_atom(TokenChars)}}.
s                 : {token, {s, TokenLine, list_to_atom(TokenChars)}}.
sa                : {token, {sa, TokenLine, list_to_atom(TokenChars)}}.
tpl               : {token, {tpl, TokenLine, list_to_atom(TokenChars)}}.
u                 : {token, {u, TokenLine, list_to_atom(TokenChars)}}.
user\sid          : {token, {user_id, TokenLine, list_to_atom(TokenChars)}}.
v                 : {token, {v, TokenLine, list_to_atom(TokenChars)}}.
yes               : {token, {yes, TokenLine, list_to_atom(TokenChars)}}.
%% ,                 : {token, {',', TokenLine, list_to_atom(TokenChars)}}.
%% =                 : {token, {'=', TokenLine, list_to_atom(TokenChars)}}.
{K}+              : {token, {string, TokenLine, TokenChars}}.
{WS}+             : skip_token.

Erlang code.
