-module(slimrt_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

test_start() ->
    application:start(slimrt),
    ?assertNot(undefined == whereis(slimrt)).
