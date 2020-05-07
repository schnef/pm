-module(pm_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    %% Thanks Fred!
    %% Create a temporary db in the priv directory
    Config1 = setup(Config),
    Config1.

end_per_suite(Config) ->
    teardown(Config),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [my_test_case].

my_test_case() -> 
    [].

my_test_case(_Config) -> 
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Setup the application. Create a temporary database and start mnesia
%% with the proper arguments.
setup(Config) ->
    zuuid:start(),
    Uuid = zuuid:string(zuuid:v1()), % generate a UUID
    Priv = ?config(priv_dir, Config),
    ct:pal("+++ Priv ~p~n", [Priv]),
    Dir = filename:join([Priv, Uuid]), % construct temporary directory
    ct:pal("+++ Dir ~p~n", [Dir]),
    ok = file:make_dir(Dir),
    %% Tell Mnesia where to put the temporary database and also
    %% increase the dump_log_write_threshold to suppress overload
    %% warnings.
    application:set_env(mnesia, dir, Dir),
    application:set_env(mnesia, dump_log_write_threshold, 5000),
    pm_db:install([node()]), % install a fresh db
    application:ensure_all_started(pm),
    [{mnesia_dir, Dir} | Config].

%% Stop pm and clean-up the temporary database
teardown(Config) ->
    Dir = ?config(mnesia_dir, Config),
    application:stop(pm),
    application:stop(mnesia),
    {ok, Filenames} = file:list_dir(Dir),
    [file:delete(filename:join([Dir, Filename])) || Filename <- Filenames],
    file:del_dir(Dir).
