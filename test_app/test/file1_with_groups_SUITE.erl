-module(file1_with_groups_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds, 1}}].

all() ->
    [dummy_test,
     {group, group1}].

groups() ->
    [{group1, [], [group_dummy_test]}].

group_dummy_test(_Config) ->
    ok.

dummy_test(_Config) ->
    ok.
