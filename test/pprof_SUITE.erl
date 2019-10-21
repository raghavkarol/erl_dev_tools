-module(pprof_SUITE).

-compile([export_all, nowarn_export_all]).

-include("pprof.hrl").

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [
     test_analyse_call_bfs,
     test_analyse_call_dfs,
     test_analyze_slowest_bfs,
     test_analyze_slowest_dfs,
     test_describe_call,
     test_max_msgs,
     test_pprof,
     test_pprof_top_calls,
     test_reset,
     test_slowest
    ].


init_per_testcase(test_max_msgs, Config) ->
    Config;

init_per_testcase(_TestCase, Config) ->
    {ok, _} = pprof:start(100),
    Config.

end_per_testcase(test_max_msgs, _Config) ->
    ok;

end_per_testcase(_TestCase, _Config) ->
    pprof:stop(),
    ok.

test_max_msgs(_Config) ->
    try
        pprof:start(4),
        pprof:add(pprof_test1),
        pprof_test1:max_msgs(),
        timer:sleep(1000),
        [Process] = ets:tab2list(pprof_result),
        #process{
           time = CallsTime,
           count = CallsCount,
           calls = Calls} = Process,
        4 = maps:size(Calls),
        4 = maps:size(CallsCount),
        4 = maps:size(CallsTime),
        ok
    after
        pprof:stop()
    end,
    ok.

test_pprof(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:f1(),
    timer:sleep(1000),
    [Process] = ets:tab2list(pprof_result),
    #process{
       time = CallsTime,
       count = CallsCount,
       pid = Pid, calls = Calls} = Process,
    Pid = self(),
    6 = maps:size(Calls),
    3 = maps:size(CallsTime),
    3 = maps:size(CallsCount),

    Calls1 = maps:values(Calls),
    1 = length([1 || #call{mfa={_, f1, _}} <- Calls1]),
    1 = length([1 || #call{mfa={_, f2, _}} <- Calls1]),
    4 = length([1 || #call{mfa={_, f3, _}} <- Calls1]),
    ok.

test_reset(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:f1(),
    timer:sleep(1000),
    [_] = ets:tab2list(pprof_result),
    pprof:reset(),
    [] = ets:tab2list(pprof_result),
    ok.

test_pprof_top_calls(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:f1(),
    timer:sleep(1000),

    Top1 = pprof:top_calls(accumulated_time, 3),
    3 = length(Top1),
    assert_reverse_sorted([V || {_K, V } <- Top1]),

    Top2 = pprof:top_calls(accumulated_time, 10),
    4 = length(Top2),
    assert_reverse_sorted([V || {_K, V } <- Top2]),

    Top3 = pprof:top_calls(count, 3),
    3 = length(Top3),
    assert_reverse_sorted([V || {_K, V } <- Top3]),

    Top4 = pprof:top_calls(count, 10),
    4 = length(Top4),
    assert_reverse_sorted([V || {_K, V } <- Top4]),
    ok.

test_slowest(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:slowest(),
    timer:sleep(1000),

    Slowest1 = pprof:slowest(3),
    3 = length(Slowest1),
    assert_reverse_sorted([T || #call{time = T} <- Slowest1]),

    Slowest2 = pprof:slowest(10),
    7 = length(Slowest2),
    assert_reverse_sorted([T || #call{time = T} <- Slowest2]),
    ok.

test_analyze_slowest_bfs(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:slowest(),
    timer:sleep(1000),
    CG = pprof:analyse_slowest(bfs),
    ct:pal("GG: ~p", [CG]),
    PP = pprof:pretty_print(CG),
    ct:pal("~s", [PP]),
    ok.

test_analyze_slowest_dfs(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:slowest(),
    timer:sleep(1000),
    CG = pprof:analyse_slowest(dfs),
    ct:pal("CG: ~p", [CG]),
    PP = pprof:pretty_print(CG),
    ct:pal("~s", [PP]),
    ok.

test_describe_call(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:describe_call(lists:seq($a, $z)),
    timer:sleep(1000),
    [#process{calls = Calls}] = ets:tab2list(pprof_result),
    [Id|_] = maps:keys(Calls),
    Call = pprof:describe_call(Id),
    PP = pprof:pretty_print(Call),
    ct:pal("~s", [PP]),
    ok.

test_analyse_call_bfs(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:slowest(),
    timer:sleep(1000),
    [#call{id = Id}] = pprof:slowest(1),
    Call = pprof:analyse_call(Id, bfs),
    PP = pprof:pretty_print(Call),
    ct:pal("~s", [PP]),
    ok.

test_analyse_call_dfs(_Config) ->
    pprof:add(pprof_test1),
    pprof_test1:slowest(),
    timer:sleep(1000),
    [#call{id = Id}] = pprof:slowest(1),
    Call = pprof:analyse_call(Id, dfs),
    PP = pprof:pretty_print(Call),
    ct:pal("~s", [PP]),
    ok.



%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------
assert_reverse_sorted(L) ->
    L = lists:reverse(lists:sort(L)).

assert_time_within_5msecs(Expected, Actual) ->
    assert_within_tolerance(Expected, Actual, 30).

assert_within_tolerance(Expected, Actual, Tolerance) ->
    if abs(Actual - Expected) =< Tolerance ->
            ok;
       true ->
            error({exceeded_tolerance, Expected, Actual})
    end.

eventually(Expected, Fun) ->
    eventually(Fun, Expected, Fun(), 0, 16).

eventually(_Fun, V, V, _N, _Max) ->
    ok;

eventually(Fun, Expected, Actual, N, Max) when N < Max ->
    timer:sleep(100),
    eventually(Fun, Fun(), Actual, N+1, Max);

eventually(Fun, Expected, Actual, Max, Max) ->
    error({max_retries, Expected, Actual}).
