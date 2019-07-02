%% See http://relops.com/blog/2014/01/13/leex_and_yecc/

Definitions.

L   = [A-Za-z0-9_]
K   = [A-Za-z0-9_\s]
WS  = ([\000-\s]|//.*)

Rules.

active       : {token, {active, TokenLine, list_to_atom(TokenChars)}}.
all          : {token, {all, TokenLine, list_to_atom(TokenChars)}}.
and          : {token, {aand, TokenLine, list_to_atom(TokenChars)}}.
any          : {token, {any, TokenLine, list_to_atom(TokenChars)}}.
as           : {token, {as, TokenLine, list_to_atom(TokenChars)}}.
ascendants   : {token, {ascendants, TokenLine, list_to_atom(TokenChars)}}.
assign       : {token, {assign, TokenLine, list_to_atom(TokenChars)}}.
assignment   : {token, {assignment, TokenLine, list_to_atom(TokenChars)}}.
attribute    : {token, {attribute, TokenLine, list_to_atom(TokenChars)}}.
base         : {token, {base, TokenLine, list_to_atom(TokenChars)}}.
choice       : {token, {choice, TokenLine, list_to_atom(TokenChars)}}.
class        : {token, {class, TokenLine, list_to_atom(TokenChars)}}.
complement   : {token, {complement, TokenLine, list_to_atom(TokenChars)}}.
create       : {token, {create, TokenLine, list_to_atom(TokenChars)}}.
delete       : {token, {delete, TokenLine, list_to_atom(TokenChars)}}.
deny         : {token, {deny, TokenLine, list_to_atom(TokenChars)}}.
do           : {token, {do, TokenLine, list_to_atom(TokenChars)}}.
each         : {token, {each, TokenLine, list_to_atom(TokenChars)}}.
exists       : {token, {exists, TokenLine, list_to_atom(TokenChars)}}.
grant        : {token, {grant, TokenLine, list_to_atom(TokenChars)}}.
if           : {token, {iif, TokenLine, list_to_atom(TokenChars)}}.
in           : {token, {in, TokenLine, list_to_atom(TokenChars)}}.
intersection : {token, {inters, TokenLine, list_to_atom(TokenChars)}}.
intrasession : {token, {intras, TokenLine, list_to_atom(TokenChars)}}.
like         : {token, {like, TokenLine, list_to_atom(TokenChars)}}.
name         : {token, {name, TokenLine, list_to_atom(TokenChars)}}.
new          : {token, {new, TokenLine, list_to_atom(TokenChars)}}.
not          : {token, {nnot, TokenLine, list_to_atom(TokenChars)}}.
object       : {token, {object, TokenLine, list_to_atom(TokenChars)}}.
of           : {token, {oof, TokenLine, list_to_atom(TokenChars)}}.
on           : {token, {on, TokenLine, list_to_atom(TokenChars)}}.
operation    : {token, {operation, TokenLine, list_to_atom(TokenChars)}}.
operations   : {token, {operations, TokenLine, list_to_atom(TokenChars)}}.
performs     : {token, {performs, TokenLine, list_to_atom(TokenChars)}}.
policy       : {token, {policy, TokenLine, list_to_atom(TokenChars)}}.
process      : {token, {process, TokenLine, list_to_atom(TokenChars)}}.
property     : {token, {property, TokenLine, list_to_atom(TokenChars)}}.
representing : {token, {representing, TokenLine, list_to_atom(TokenChars)}}.
rule         : {token, {rule, TokenLine, list_to_atom(TokenChars)}}.
rules        : {token, {rules, TokenLine, list_to_atom(TokenChars)}}.
session      : {token, {session, TokenLine, list_to_atom(TokenChars)}}.
script       : {token, {script, TokenLine, list_to_atom(TokenChars)}}.
then         : {token, {then, TokenLine, list_to_atom(TokenChars)}}.
to           : {token, {to, TokenLine, list_to_atom(TokenChars)}}.
user         : {token, {user, TokenLine, list_to_atom(TokenChars)}}.
when         : {token, {wwhen, TokenLine, list_to_atom(TokenChars)}}.
with         : {token, {with, TokenLine, list_to_atom(TokenChars)}}.

"{K}+"       : S = strip(TokenChars, TokenLen),
               {token, {str, TokenLine, S}}.
{L}+         : {token, {str, TokenLine, TokenChars}}.
[(),:]       : {token, {list_to_atom(TokenChars), TokenLine}}.
{WS}+        : skip_token.

Erlang code.

strip(TokenChars, TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).
