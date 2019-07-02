%% See http://relops.com/blog/2014/01/13/leex_and_yecc/

%% For the Harmonia implementation have a look at: nist/csd/pm/server/parser/RuleParser.java

%% TODO: Check this parser against the Harmonia one.

Header "%% Copyright (C) 2019"
"%% @private"
"%% @Author Frans Schneider".

Nonterminals 
'<action>' 
'<any obj>'
'<any op>'
'<any pc>'
'<any user>'
'<arg list>'
'<assign action>'
'<assign as>'
'<assign like>'
'<assign to containers>'
'<assign to>' 
'<assign what>'
'<attr container>'
'<base container>'
'<class>'
'<cond entity>'
'<condition>'
'<conditional action>'
'<container set>'
'<container subspec>'
'<container>'
'<create action>'
'<create what>'
'<create where>'
'<delete action>'
'<delete assignment subaction>'
'<delete deny subaction>'
'<delete rules subaction>'
'<delete subaction>'
'<denied op set>'
'<deny action>'
'<deny on>'
'<deny to>'
'<deny what>'
'<each pc>'
'<event pattern>'
'<grant action>'
'<grant on>'
'<grant to>'
'<grant what>'
'<granted op set>'
'<label set>'
'<model entity>'
'<name or function call>'
'<oattr>'
'<obj as container>'
'<obj container set>'
'<obj container>'
'<obj or attr or class set>'
'<obj or attr or class>'
'<obj spec>'
'<obj subspec>'
'<obj>'
'<op set>'
'<op spec>'
'<op subspec>'
'<op>'
'<pc set>'
'<pc spec>'
'<pc subspec>'
'<pc>'
'<policy container>'
'<process>'
'<property set>'
'<represented entity>'
'<representing what>'
'<response>'
'<rule>'
'<rules>'
'<session>'
'<script>'
'<uattr spec>' 
'<uattr>' 
'<user or attr set>'
'<user or attr>'
'<user spec>'
'<user>'
'<with properties>'
.

Terminals '(' ')' ',' ':' str
active aand any as ascendants assign
assignment attribute base class complement create delete deny
do each exists grant iif in inters intras like
nnot object oof on operation operations performs policy process property
representing rule rules script session then to user wwhen with.

%% all choice name new

Rootsymbol '<script>'.

'<script>'                      -> script str '<rules>'
				       : {script, unwrap('$2'), '$3'}.
'<rules>'                       -> '<rule>' '<rules>'
				       : ['$1' | '$2'].
'<rules>'                       -> '$empty'
				       : [].
'<rule>'                        -> str ':' wwhen '<event pattern>' do '<response>'
				       : {rule, '$4', '$6'}.
'<rule>'                        -> wwhen '<event pattern>' do '<response>'
				       : {rule, '$2', '$4'}.
'<event pattern>'               -> '<user spec>' '<pc spec>' '<op spec>' '<obj spec>'
				       : {pattern, '$1','$2','$3','$4'}.
'<user spec>'                   -> '<user>'
				       : '$1'.
'<user spec>'                   -> '<any user>'
				       : '$1'.
'<user spec>'                   -> '<session>'
				       : '$1'.
'<user spec>'                   -> '<process>'
				       : '$1'.
'<user spec>'                   -> '$empty'
				       : undefined.
'<user>'                        -> user str
				       : {u, unwrap('$2')}.
'<user>'                        -> str
				       : {u, unwrap('$1')}.
'<any user>'                    -> any user oof '<user or attr set>'
				       : {any_u, '$4'}.
'<any user>'                    -> any oof '<user or attr set>'
				       : {any_u, '$3'}.
'<any user>'                    -> any user
				       : {any_u, []}.
'<any user>'                    -> any
				       : {any_u, []}.
'<user or attr set>'            -> '<user or attr>' ',' '<user or attr set>'
				       : ['$1'] ++ '$3'.
'<user or attr set>'            -> '<user or attr>'
				       : ['$1'].
'<user or attr>'                -> '<user>'
				       : '$1'.
'<user or attr>'                -> '<uattr>'
				       : '$1'.
'<uattr>'                       -> active attribute str
				       : {ua, unwrap('$3'), [{active, true}]}.
'<uattr>'                       -> attribute str
				       : {ua, unwrap('$2'), []}.
'<process>'                     -> process str
				       : {process, unwrap('$2')}.
'<session>'                     -> session str
				       : {session, unwrap('$2')}.
'<pc spec>'                     -> active in '<pc subspec>'
				       : {pc, '$3', [{active, true}]}.
'<pc spec>'                     -> in '<pc subspec>'
				       : {pc, '$2', []}.
'<pc spec>'                     -> active
				       : {pc, undefined, [{active, true}]}.
'<pc spec>'                     -> '$empty'
				       : undefined.
'<pc subspec>'                  -> '<pc>'
				       : '$1'.
'<pc subspec>'                  -> '<any pc>'
				       : '$1'.
'<pc subspec>'                  -> '<each pc>'
				       : '$1'.
'<pc>'                          -> policy str
				       : unwrap('$2').
'<pc>'                          -> str
				       : unwrap('$1').
'<any pc>'                      -> any policy oof '<pc set>'
				       : {any_pc, '$4'}.
'<any pc>'                      -> any oof '<pc set>'
				       : {any_pc, '$3'}.
'<any pc>'                      -> any policy
				       : {any_pc, undefined}.
'<any pc>'                      -> any
				       : {any_pc, undefined}.
'<each pc>'                     -> each policy oof '<pc set>'
				       : {each_pc, '$4'}.
'<each pc>'                     -> each oof '<pc set>'
				       : {each_pc, '$3'}.
'<each pc>'                     -> each policy
				       : {each_pc, undefined}.
'<each pc>'                     -> each
				       : {each_pc, undefined}.
'<pc set>'                      -> '<pc>' ',' '<pc set>'
				       : ['$1'] ++ '$3'.
'<pc set>'                      -> '<pc>'
				       : ['$1'].
'<op spec>'                     -> performs '<op subspec>'
				       : {op, '$2'}.
'<op subspec>'                  -> '<op>'
				       : '$1'.
'<op subspec>'                  -> '<any op>'
				       : '$1'.
'<op>'                          -> operation str
				       : unwrap('$2').
'<op>'                          -> str
				       : unwrap('$1').
'<any op>'                      -> any operation oof '<op set>'
				       : '$4'.
'<any op>'                      -> any oof '<op set>'
				       : '$3'.
'<any op>'                      -> any operation
				       : [].
'<any op>'                      -> any
				       : [].
'<op set>'                      -> '<op>' ',' '<op set>'
				       : ['$1'] ++ '$3'.
'<op set>'                      -> '<op>'
				       : ['$1'].
'<obj spec>'                    -> on '<obj subspec>' '<container subspec>'
				       : {obj, '$2', '$3'}.
'<obj spec>'                    -> '$empty'
				       : undefined.
'<obj subspec>'                 -> '<obj>'
				       : '$1'.
'<obj subspec>'                 -> '<any obj>'
				       : '$1'.
'<obj>'                         -> object str
				       : {o, unwrap('$2')}.
'<obj>'                         -> str
				       : {o, unwrap('$1')}.
'<any obj>'                     -> any object
				       : {o, undefined}.
'<any obj>'                     -> any
				       : {o, undefined}.
'<container subspec>'           -> oof '<obj or attr or class set>'
				       : '$2'.
'<container subspec>'           -> '$empty'
				       : undefined. 
'<obj or attr or class set>'    -> '<obj or attr or class>' ',' '<obj or attr or class set>'
				       : ['$1'] ++ '$3'.
'<obj or attr or class set>'    -> '<obj or attr or class>'
				       : ['$1'].
'<obj or attr or class>'        -> '<obj as container>'
				       : '$1'.
'<obj or attr or class>'        -> '<oattr>'
				       : '$1'.
'<obj or attr or class>'        -> '<class>'
				       : '$1'.
'<obj as container>'            -> object str
				       : {o, unwrap('$2')}.
'<obj as container>'            -> str
				       : {o, unwrap('$1')}.
'<class>'                       -> class str
				       : {class, unwrap('$2')}.
'<oattr>'                       -> attribute str
				       : {oa, unwrap('$2')}.
'<response>'                    -> '<conditional action>'
				       : {response, '$1'}.
'<response>'                    -> '$empty'
				       : undefined.
'<conditional action>'          -> iif '<condition>' then '<action>'
				       : {'$2', '$4'}.
'<conditional action>'          -> '<action>'
				       : '$1'.
'<condition>'                   -> nnot '<cond entity>' exists
				       : {'not', '$2'}.
'<condition>'                   -> '<cond entity>' exists
				       : '$1'.
'<cond entity>'                 -> user attribute '<name or function call>'
				       : {ua, '$3'}.
'<cond entity>'                 -> user '<name or function call>'
				       : {u, '$2'}.
'<cond entity>'                 -> object attribute '<name or function call>'
				       : {oa, '$3'}.
'<cond entity>'                 -> object '<name or function call>'
				       : {o, '$2'}.
'<cond entity>'                 -> policy '<name or function call>'
				       : {pc, '$2'}.
'<action>'                      -> '<assign action>'
				       : '$1'.
'<action>'                      -> '<grant action>'
				       : '$1'.
'<action>'                      -> '<create action>'
				       : '$1'.
'<action>'                      -> '<deny action>'
				       : '$1'.
'<action>'                      -> '<delete action>' 
				       : '$1'.
'<assign action>'               -> assign '<assign what>' '<assign to>'
				       : {assign, '$2', '$3'}.
'<assign what>'                 -> user attribute '<name or function call>'
				       : {ua, '$2'}.
'<assign what>'                 -> user '<name or function call>'
				       : {u, '$2'}.
'<assign what>'                 -> object attribute '<name or function call>'
				       : {oa, '$2'}.
'<assign what>'                 -> object '<name or function call>'
				       : {o, '$2'}.
'<assign to>'                   -> '<assign like>'
				       : '$1'.
'<assign to>'                   -> '<assign as>'
				       : '$1'.
'<assign to>'                   -> '<assign to containers>'
				       : '$1'.
'<assign like>'                 -> like '<model entity>'
				       : {like, '$2'}.
'<assign as>'                   -> as '<model entity>'
				       : {as, '$2'}.
'<model entity>'                -> user attribute '<name or function call>'
				       : {ua, '$2'}.
'<model entity>'                -> user '<name or function call>'
				       : {u, '$2'}.
'<model entity>'                -> object attribute '<name or function call>'
				       : {oa, '$2'}.
'<model entity>'                -> object '<name or function call>'
				       : {o, '$2'}.
'<assign to containers>'        -> to '<container set>'
				       : {to, '$2'}.
'<container set>'               -> '<container>' ',' '<container set>'
				       : ['$1'] ++ '$3'.
'<container set>'               -> '<container>'
				       : ['$1'].
'<container>'                   -> '<base container>'
				       : '$1'.
'<container>'                   -> '<policy container>'
				       : '$1'.
'<container>'                   -> '<attr container>'
				       : '$1'.
'<base container>'              -> base 
				       : base.
'<policy container>'            -> policy '<name or function call>'
				       : {pc, '$2'}.
'<attr container>'              -> user attribute '<name or function call>'
				       : {ua, '$2'}.
'<attr container>'              -> object attribute '<name or function call>'
				       : {oa, '$2'}.
'<name or function call>'       -> str '(' '<arg list>' ')'
				       : {func, unwrap('$1'), '$3'}.
'<name or function call>'       -> str '(' ')'
				       : {func, unwrap('$1'), []}.
'<name or function call>'       -> str
				       : {name, unwrap('$1')}.
'<arg list>'                    -> '<name or function call>' ',' '<arg list>'
				       : ['$1'] ++ '$3'.
'<arg list>'                    -> '<name or function call>'
				       : ['$1'].
'<grant action>'                -> grant '<grant to>' '<grant what>' '<grant on>'
				       : {grant, '$2', '$3', '$4'}.
'<grant to>'                    -> '<uattr spec>' ',' '<grant to>'
				       : ['$1'] ++ '$3'.
'<grant to>'                    -> '<uattr spec>'
				       : ['$1'].
'<uattr spec>'                  -> user attribute '<name or function call>'
				       : {ua, '$3'}.
'<uattr spec>'                  -> attribute '<name or function call>'
				       : {ua, '$2'}.
'<uattr spec>'                  -> '<name or function call>'
				       : {'$1'}.
'<grant what>'                  -> operation '<granted op set>'
				       : {'$2'}.
'<grant what>'                  -> operations '<granted op set>'
				       : {'$2'}.
'<granted op set>'              -> str ',' '<granted op set>'
				       : [unwrap('$1')] ++ '$3'.
'<granted op set>'              -> str
				       : [unwrap('$1')].
'<grant on>'                    -> on object attribute '<name or function call>'
				       : {oa, '$2'}.
'<grant on>'                    -> on object '<name or function call>'
				       : {o, '$2'}.
'<grant on>'                    -> '$empty'
				       : undefined.
'<create action>'               -> create '<create what>' '<representing what>' '<with properties>' '<create where>'
				       : {create, '$2', '$3', '$4', '$5'}.
'<create what>'                 -> user attribute '<name or function call>'
				       : {ua, '$2'}.
'<create what>'                 -> user '<name or function call>'
				       : {u, '$2'}.
'<create what>'                 -> object attribute '<name or function call>'
				       : {oa, '$2'}.
'<create what>'                 -> object '<name or function call>'
				       : {o, '$2'}.
'<create what>'                 -> policy '<name or function call>'
				       : {pc, '$2'}.
'<representing what>'           -> representing '<represented entity>' aand ascendants
				       : {repr, '$2', [{ascendants, true}]}.
'<representing what>'           -> representing '<represented entity>'
				       : {repr, '$2', []}.
'<representing what>'           -> '$empty'
				       : undefined.
'<represented entity>'          -> user attribute '<name or function call>'
				       : {ua, '$3'}.
'<represented entity>'          -> user '<name or function call>'
				       : {u, '$2'}.
'<represented entity>'          -> object attribute '<name or function call>'
				       : {oa, '$3'}.
'<represented entity>'          -> object '<name or function call>'
				       : {o, '$2'}.
'<represented entity>'          -> policy '<name or function call>'
				       : {pc, '$2'}.
'<represented entity>'          -> base
				       : base.
'<with properties>'             -> with property '<property set>'
				       : {props, '$2'}.
'<with properties>'             -> '$empty'
				       : undefined.
'<property set>'                -> '<name or function call>' ',' '<property set>'
				       : ['$1'] ++ '$3'.
'<property set>'                -> '<name or function call>'
				       : ['$1'].
'<create where>'                -> in '<container>'
				       : {'$2'}.
'<deny action>'                 -> deny '<deny to>' '<deny what>' '<deny on>'
				       : {deny, '$2', '$2', '$2'}.
'<deny to>'                     -> user attribute '<name or function call>' intras
				       : {ua, '$3', [{intrasession, true}]}.
'<deny to>'                     -> user '<name or function call>' intras
				       : {u, '$2', [{intrasession, true}]}.
'<deny to>'                     -> user attribute '<name or function call>'
				       : {ua, '$3', []}.
'<deny to>'                     -> user '<name or function call>'
				       : {u, '$2', []}.
'<deny to>'                     -> session '<name or function call>'
				       : {p, '$2', []}.
'<deny what>'                   -> operations '<denied op set>'
				       : {op, '$1'}.
'<deny what>'                   -> operation '<denied op set>'
				       : {op, '$2'}.
'<denied op set>'               -> str ',' '<denied op set>'
				       : [unwrap('$1')] ++ '$3'.
'<denied op set>'               -> str
				       : [unwrap('$1')].
'<deny on>'                     -> on inters oof '<obj container set>'
				       : {'$1', [{intersection, true}]}.
'<deny on>'                     -> on '<obj container set>'
				       : {'$1', []}.
'<deny on>'                     -> '$empty'
				       : undefined.
'<obj container set>'           -> '<obj container>' ',' '<obj container set>'
				       : ['$1'] ++ '$3'.
'<obj container set>'           -> '<obj container>'
				       : ['$1'].
'<obj container>'               -> complement oof object attribute '<name or function call>'
				       : {oa, '$2', [{complement, true}]}.
'<obj container>'               -> complement oof attribute '<name or function call>'
				       : {oa, '$2', [{complement, true}]}.
'<obj container>'               -> complement oof object '<name or function call>'
				       : {o, '$2', [{complement, true}]}.
'<obj container>'               -> object attribute '<name or function call>'
				       : {oa, '$2', []}.
'<obj container>'               -> attribute '<name or function call>'
				       : {oa, '$2', []}.
'<obj container>'               -> object '<name or function call>'
				       : {o, '$2', []}.
'<delete action>'               -> delete '<delete subaction>'
				       : {delete, '$2'}.
'<delete subaction>'            -> '<delete assignment subaction>'
				       : '$1'.
'<delete subaction>'            -> '<delete deny subaction>'
				       : '$1'.
'<delete subaction>'            -> '<delete rules subaction>'
				       : '$1'.
'<delete assignment subaction>' -> assignment oof '<assign what>' '<assign to containers>'
				       : {assignment, '$2', '$3'}.
'<delete deny subaction>'       -> deny '<deny to>' '<deny what>' '<deny on>'
				       : {deny, '$2', '$3', '$4'}.
'<delete rules subaction>'      -> rules '<label set>'
				       : {rule, '$2'}.
'<delete rules subaction>'      -> rule '<label set>'
				       : {rule, '$2'}.
'<label set>'                   -> str ',' '<label set>'
				       : [unwrap('$1')] ++ '$3'.
'<label set>'                   -> str
				       : [unwrap('$1')].


Erlang code.

unwrap({_,_,V}) -> V.


