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

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

is_pm({pc, User_DAG, Object_DAG}) when is_list(User_DAG), is_list(Object_DAG) ->
    true;
is_pm(_) ->
    false.

boolean(_) -> true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

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
