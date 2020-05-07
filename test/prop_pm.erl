-module(prop_pm).
-include_lib("proper/include/proper.hrl").
-include("../src/pm.hrl").
-compile(export_all).

%% pm_t is a tree structure with a pc as the root. It is the wrong way
%% round because a pm should start from the u's and os and go down to
%% one or more pc's. But, we get at least something with minimal
%% effort using the type system!
-export_type([pm_t/0]).
-type pm_t() :: {pc_t(),
		 user_dag_t(),
		 object_dag_t()}.

-type user_dag_t() :: [{ua_t(), nonempty_list(u_ua_t())}].
-type u_ua_t() :: {ua_t(), nonempty_list(u_ua_t())}
		| u_t().

-type object_dag_t() :: [{oa_t(), nonempty_list(o_oa_t())}].
-type o_oa_t() :: {oa_t(), nonempty_list(o_oa_t())}
		| o_t().

-type u_t() :: u.
-type ua_t() :: ua.
-type o_t() :: o.
-type oa_t() :: oa.
-type pc_t() :: pc.

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_pm_t() ->
    ?SETUP(fun setup/0,
	   ?FORALL(PM, ?MODULE:pm_t(),
		   begin
		       is_pm(PM)
		   end)
	  ).

%% The following tests are not real tests because they just repeat
%% the same test which won't fail. Consider this however as some
%% preludes to get to now PropEr.
prop_pc() ->
    ?SETUP(fun setup/0,
	   ?FORALL(PC, pc(),
		   begin
		       {ok, PC1} = pm_pap:c_pc(PC),
		       io:format("pc ~p~n~p~n", [PC, PC1]),
		       false
		   end)
	  ).

prop_c_pc() ->
    ?SETUP(fun setup/0,
	   ?FORALL(C_PC, c_pc(),
		   begin
		       {ok, PC} = eval(C_PC),
		       io:format("c_pc ~p~n~p~n", [C_PC, PC]),
		       false
		   end)
	  ).

prop_sc_pc() ->
    ?SETUP(fun setup/0,
	   ?FORALL(SC_PC, sc_pc(),
		   begin
		       {ok, PC} = SC_PC,
		       io:format("sc_pc ~p~n~p~n", [SC_PC, PC]),
		       false
		   end)
	  ).

prop_ua() ->
    ?SETUP(fun setup/0,
	   ?FORALL({{ok, PC}, UAs}, {c_pc(), list(ua())},
		   begin
		       [{ok, #ua{}} = pm_pap:c_ua_in_pc(UA, PC) || UA <- UAs],
		       length(UAs) + 1 =:= length(pm_pap:elements(PC))
		   end)
	  ).

prop_u() ->
    ?SETUP(fun setup/0,
	   ?FORALL({PC, UAs, Us}, {pc(), non_empty(list(ua())), list(u())},
		   begin
		       {ok, PC1} = pm_pap:c_pc(PC),
		       [UA1 | UAsx] = [begin
					   {ok, #ua{} = UA1} = pm_pap:c_ua_in_pc(UA, PC1),
					   UA1
				       end || UA <- UAs],
		       Us1 = [begin
				  {ok, #u{} = U1} = pm_pap:c_u_in_ua(U, UA1),
				  U1
			      end || U <- Us],
		       Assigns = [ok = pm_pap:c_u_to_ua(U1, UAx) || UAx <- UAsx, U1 <- Us1],
		       collect({'u->ua', to_range(100, length(Assigns) + length(Us))},
			       length(Us) + length(UAs) + 1 =:= length(pm_pap:elements(PC1))
			       andalso
			       length(Us) =:= length(pm_pap:users(UA1)))
		   end)
	  ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

to_range(M, N) ->
    Base = N div M,
    {Base * M, (Base + 1) * M}.

is_pm({pc, User_DAG, Object_DAG}) when is_list(User_DAG), is_list(Object_DAG) ->
    true;
is_pm(_) ->
    false.

boolean(_) -> true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% Generate an instance of the pc, u, ua, o o and oa record.
pc() ->
    ?LET(Value, term(), #pc{value = Value}).

u() ->
    ?LET(Value, term(), #u{value = Value}).

ua() ->
    ?LET(Value, term(), #ua{value = Value}).

o() ->
    ?LET(Value, term(), #o{value = Value}).

oa() ->
    ?LET(Value, term(), #oa{value = Value}).

%% Make a call to the system to generate a PC
c_pc() ->
    ?LET(PC, pc(), {'call', pm_pap, c_pc, [PC]}).

sc_pc() ->
    ?LET(PC, pc(), {'$call', pm_pap, c_pc, [PC]}).

%% @doc Setup the application, including the database, for running the
%% tests. This function returns a tear down function.
%%
%% Use a temporary databse to run the tests.
%% Start the `pm' application.
%% Afterwards, shut down the `pm' application and
%% remove the temporary database
setup() ->
    zuuid:start(),
    {ok, Cwd} = file:get_cwd(), % current working directory
    Uuid = zuuid:string(zuuid:v1()), % generate a UUID
    Dir = filename:join([Cwd, "test", Uuid]), % construct temporary directory
    ok = file:make_dir(Dir),
    %% Tell Mnesia where to put the temporary database and also
    %% increase the dump_log_write_threshold to suppress overload
    %% warnings.
    application:set_env(mnesia, dir, Dir),
    application:set_env(mnesia, dump_log_write_threshold, 5000),
    pm_db:install([node()]), % install a fresh db
    application:ensure_all_started(pm),
    fun() ->
	    application:stop(pm),
	    application:stop(mnesia),
	    {ok, Filenames} = file:list_dir(Dir),
	    [file:delete(filename:join([Dir, Filename])) || Filename <- Filenames],
	    file:del_dir(Dir)
    end.
