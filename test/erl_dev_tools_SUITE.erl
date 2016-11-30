%%%-------------------------------------------------------------------
%%% @author Raghav Karol <raghav.karol@gmail.com>
%%% @copyright (C) 2016, Raghav Karol
%%% @doc
%%%
%%% @end
%%% Created : 28 Nov 2016 by Raghav Karol <raghav.karol@gmail.com>
%%%-------------------------------------------------------------------
-module(erl_dev_tools_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

all() ->
    util:all_tests(?MODULE).


top_supervisor_start_test(_Config) ->
    erl_dev_tools_sup:start_link(),
    true = is_pid(whereis(fswatch_buf)),
    supervisor:start_child(fswatch_sup, ["/tmp"]),
    supervisor:start_child(fswatch_sup, ["/tmp"]),
    [_, _] = supervisor:which_children(fswatch_sup),
    exit(whereis(erl_dev_tools_sup), normal),
    undefined = fswatch_SUITE:retry(fun() -> whereis(erl_dev_tools_sup) end,
                      _Expected = undefined),
    undefined = fswatch_SUITE:retry(fun() -> whereis(fswatch_sup) end,
                      _Expected = undefined),
    ok.

application_start_stop_test(_Config) ->
    ok = application:start(erl_dev_tools),
    [_, _, _] = supervisor:which_children(erl_dev_tools_sup),
    [{_, Pid, _, _}] = supervisor:which_children(fswatch_sup),
    OsPid = fswatch:port_os_pid(Pid),
    application:stop(erl_dev_tools),
    false = fswatch_SUITE:retry(fun() -> fswatch_SUITE:is_os_process_alive(OsPid) end, false),
    undefined = fswatch_SUITE:retry(fun() -> whereis(fswatch_sup) end,
                                    undefined),
    ok.
