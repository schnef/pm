-module(pm_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    %% Thanks Fred!
    %% Create a temporary db in the priv directory
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    pm_db:install([node()]),
    application:start(mnesia),
    application:start(pm),
    Config.

end_per_suite(_Config) ->
    application:stop(pm),
    application:stop(mnesia),
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
