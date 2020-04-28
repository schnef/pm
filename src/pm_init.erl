-module(pm_init).

-export([main/1]).

main(_) ->
    pm_db:install(),
    init:stop().
