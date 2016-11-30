-module(file1_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds, 1}}].

all() ->
    [dummy_test].

dummy_test(_Config) ->
    ok.
