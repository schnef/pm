%% See http://relops.com/blog/2014/01/13/leex_and_yecc/

Header "%% Copyright (C) 2019"
"%% @private"
"%% @Author Frans Schneider".

Nonterminals 
'<cmd_lines>' '<cmd_line>' '<obj class>' '<yesno>' '<base node type>' 
'<entity type>' '<entity type1>''<entity type2>''<deny type>' '<user or attr>'.


Terminals ',' '='
'Connector' 'Directory' 'File' 'Operation set' 'Policy class' 'User attr' 'User'
'Object' 'Object attr' a across_sess add app as asg b c cb comps conts deny eml 
h fn intra_sess key ks no o ob oc op p pm prop s sa tpl u user_id v yes string.

%% Not documented: eml fn conts tpl key comps o v

Rootsymbol '<cmd_lines>'.

'<cmd_lines>'      -> '<cmd_line>' '<cmd_lines>'.
'<cmd_lines>'      -> '<cmd_line>'.
'<cmd_line>'       -> add a string a string.
'<cmd_line>'       -> add a string as string.
'<cmd_line>'       -> add a string c pm.
'<cmd_line>'       -> add a string p string.
'<cmd_line>'       -> add a string u string.
'<cmd_line>'       -> add app string string string string string.
'<cmd_line>'       -> add as string sa string.
'<cmd_line>'       -> add as string.
'<cmd_line>'       -> add b string b string.
'<cmd_line>'       -> add b string c pm.
'<cmd_line>'       -> add b string deny string.
'<cmd_line>'       -> add b string deny string string.
'<cmd_line>'       -> add b string p string.
'<cmd_line>'       -> add cb string deny string.
'<cmd_line>'       -> add comps string b string.
'<cmd_line>'       -> add deny string '<deny type>' '<user or attr>' string '<yesno>'.
'<cmd_line>'       -> add eml string string string string string string u string.
'<cmd_line>'       -> add key string b string.
'<cmd_line>'       -> add key string tpl string.
'<cmd_line>'       -> add ks string string h string u string.
'<cmd_line>'       -> add ob string '<obj class>' '<yesno>' string string '<base node type>' string.
'<cmd_line>'       -> add oc string.
'<cmd_line>'       -> add op string deny string.
'<cmd_line>'       -> add op string oc string.
'<cmd_line>'       -> add op string s string.
'<cmd_line>'       -> add p string c pm.
'<cmd_line>'       -> add prop string '<entity type>' string.
'<cmd_line>'       -> add s string oc string a string.
'<cmd_line>'       -> add s string oc string b string.
'<cmd_line>'       -> add sa string.
'<cmd_line>'       -> add tpl string b string.
'<cmd_line>'       -> add tpl string conts string.
'<cmd_line>'       -> add u string a string.
'<cmd_line>'       -> add u string fn string a string.
'<cmd_line>'       -> add u string u pm.
'<cmd_line>'       -> asg '<entity type1>' string '<entity type2>' string.
'<obj class>'      -> 'Connector'.
'<obj class>'      -> 'Directory'.
'<obj class>'      -> 'File'.
'<obj class>'      -> 'Object attr'.
'<obj class>'      -> 'Object'.
'<obj class>'      -> 'Operation set'.
'<obj class>'      -> 'Policy class'.
'<obj class>'      -> 'User attr'.
'<obj class>'      -> 'User'.
'<yesno>'          -> no.
'<yesno>'          -> yes.
'<base node type>' -> b. 
'<base node type>' -> c. 
'<base node type>' -> p. 
'<entity type>'    -> a.
'<entity type>'    -> b.
'<entity type>'    -> p.
'<entity type>'    -> v.
'<entity type1>'   -> a.
'<entity type1>'   -> b.
'<entity type1>'   -> o.
'<entity type1>'   -> s.
'<entity type1>'   -> u.
'<entity type2>'   -> a.
'<entity type2>'   -> b.
'<entity type2>'   -> p.
'<entity type2>'   -> s.
'<deny type>'      -> across_sess.
'<deny type>'      -> intra_sess.
'<deny type>'      -> user_id.
'<user or attr>'   -> a. 
'<user or attr>'   -> u. 

Erlang code.

unwrap({_,_,V}) -> V.


