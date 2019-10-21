-module(pprof_test1).

-compile([export_all, nowarn_export_all]).

f1() ->
    f2(),
    ok.

f2() ->
    f3(),
    f3(),
    f3(),
    f3(),
    ok.

f3() ->
    timer:sleep(25),
    ok.


max_msgs() ->
    max_msgs1(),
    max_msgs2(),
    max_msgs3(),
    max_msgs4(),
    max_msgs5(),
    max_msgs6(),
    max_msgs7().


max_msgs1() ->
    ok.
max_msgs2() ->
    ok.
max_msgs3() ->
    ok.
max_msgs4() ->
    ok.
max_msgs5() ->
    ok.
max_msgs6() ->
    ok.
max_msgs7() ->
    ok.

slowest() ->
    timer:sleep(10),
    slowest1(),
    slowest(1),
    slowest(2),
    slowest(3),
    slowest(4),
    slowest(5),
    ok.

slowest1() ->
    slowest1_1(),
    slowest1_2(),
    ok.

slowest1_1() ->
    slowest1_1_1(),
    ok.

slowest1_2() ->
    ok.

slowest1_1_1() ->
    ok.

slowest(N) ->
    timer:sleep(1 * 10),
    if N == 1 ->
            max_msgs();
       true ->
            ok
    end,
    ok.

describe_call(_List) ->
    timer:sleep(100),
    the_result.
