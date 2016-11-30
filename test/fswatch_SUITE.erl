%%%-------------------------------------------------------------------
%%% @author Raghav Karol <raghav.karol@gmail.com>
%%% @copyright (C) 2016, Raghav Karol
%%% @doc
%%%
%%% @end
%%% Created : 12 Nov 2016 by Raghav Karol <rk@issuu.com>
%%%-------------------------------------------------------------------
-module(fswatch_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    {ok, Cwd} = file:get_cwd(),
    WatchDir = Cwd ++ "/watch_dir/", % Trailing slash is required by ensure_dir
    filelib:ensure_dir(WatchDir),
    [{watch_dir, WatchDir} | Config].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    application:stop(erl_dev_tools),
    undefined = fswatch_SUITE:retry(fun() -> whereis(fswatch_sup) end,
                                    undefined),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    util:all_tests(?MODULE).

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
fswatch_sup_start_test(_Config) ->
    fswatch_sup:start_link(),
    [] = supervisor:which_children(fswatch_sup),

    [supervisor:start_child(fswatch_sup, ["/tmp"]) || _ <- lists:seq(1, 8)],
    Pids = [Pid || {_, Pid, _, _} <- supervisor:which_children(fswatch_sup)],
    retry(fun() -> length(supervisor:which_children(fswatch_sup)) end, 8),
    exit(whereis(fswatch_sup), normal),
    [retry(fun() -> process_info(Pid) end, undefined) || Pid <- Pids],
    ok.

fswatch_restart_test(_Config) ->
    ok = application:start(erl_dev_tools),

    [{_, Pid1, _, _}] = supervisor:which_children(fswatch_sup),
    OsPid1 = fswatch:port_os_pid(Pid1),
    os:cmd("kill " ++ integer_to_list(OsPid1)),

    undefined = retry(fun() -> process_info(Pid1) end, undefined),
    false = retry(fun() -> is_os_process_alive(OsPid1) end, false),

    [{_, Pid2, _, _}] = supervisor:which_children(fswatch_sup),
    OsPid2 = fswatch:port_os_pid(Pid2),
    true = retry(fun() -> is_os_process_alive(OsPid2) end, true),
    ok = application:stop(erl_dev_tools),
    ok.

fwatch_create_file_test(Config) ->
    WatchDir = ?config(watch_dir, Config),
    erl_dev_tools_sup:start_link(),
    supervisor:start_child(fswatch_sup, [WatchDir]),
    CreatedFiles = create_files(WatchDir, _Count = 3),
    Changes = get_changes(length(CreatedFiles)),
    CreatedFiles = Changes,
    exit(whereis(erl_dev_tools_sup), normal),
    retry(fun() -> whereis(erl_dev_tools_sup) end, undefined),
    ok.

fswatch_update_file_test(Config) ->
    WatchDir = ?config(watch_dir, Config),
    CreatedFiles = create_files(WatchDir, _Count = 3),
    erl_dev_tools_sup:start_link(),
    supervisor:start_child(fswatch_sup, [WatchDir]),
    UpdatedFile = update_file(pick(CreatedFiles)),
    [Change] = get_changes(_ExpectedCount = 1),
    UpdatedFile = Change,
    exit(whereis(erl_dev_tools_sup), normal),
    retry(fun() -> whereis(erl_dev_tools_sup) end, undefined),
    ok.

fswatch_delete_file_test(Config) ->
    WatchDir = ?config(watch_dir, Config),
    CreatedFiles = create_files(WatchDir, _Count = 3),
    erl_dev_tools_sup:start_link(),
    supervisor:start_child(fswatch_sup, [WatchDir]),
    DeletedFile = delete_file(pick(CreatedFiles)),
    [Change] = get_changes(_ExpectedCount = 1),
    DeletedFile = Change,
    exit(whereis(erl_dev_tools_sup), normal),
    retry(fun() -> whereis(erl_dev_tools_sup) end, undefined),
    ok.

fwatch_flush_test(Config) ->
    WatchDir = ?config(watch_dir, Config),
    erl_dev_tools_sup:start_link(),
    supervisor:start_child(fswatch_sup, [WatchDir]),
    CreatedFiles = create_files(WatchDir, _Count = 10),
    Changes = get_changes(length(CreatedFiles)),
    Changes = fswatch_buf:flush(),
    [] = fswatch_buf:flush(),
    CreatedFiles = Changes,
    exit(whereis(erl_dev_tools_sup), normal),
    retry(fun() -> whereis(erl_dev_tools_sup) end, undefined),
    ok.


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------
pick(List) ->
    N = random:uniform(length(List)),
    lists:nth(N, List).

get_changes(ExpectedCount) ->
    get_changes([], ExpectedCount).

get_changes(Changes, ExpectedCount) when length(Changes) =:= ExpectedCount ->
    lists:sort(Changes);
get_changes(Changes, ExpectedCount) when length(Changes) < ExpectedCount ->
    Changes1 = Changes ++ fswatch_buf:wait_get_changes(),
    %% Remove get_changes doesn't empty buffered changes so it can
    %% return previously seen changes
    Changes2 = lists:usort(Changes1),
    get_changes(Changes2, ExpectedCount).

create_files(WatchDir, Count) ->
    Paths = [WatchDir ++ "new_file_" ++ integer_to_list(N) ++ ".erl"
             || N <- lists:seq(1, Count)],
    [create_file(Path) || Path <- Paths],
    lists:sort(Paths).

create_file(Path) ->
    os:cmd("touch " ++ Path),
    Path.

update_file(Path) ->
    os:cmd("touch " ++ Path),
    Path.

delete_file(Path) ->
    os:cmd("rm " ++ Path),
    Path.

is_os_process_alive(OsPid) ->
    PsOut = os:cmd("pgrep -f "
                   ++ fswatch:find_executable()
                   ++ " | grep "
                   ++ integer_to_list(OsPid)),
    case string:strip(PsOut, both, $\n) of
        [] ->
            false;
        _ ->
            true
    end.

fswatch_os_pids() ->
    OsPids = os:cmd("pgrep -f " ++ fswatch:find_executable()),
    [list_to_integer(P) || P <- string:tokens(OsPids, "\n")].

retry(Fun, Result) ->
   retry(_Max = 10,  _N = 1, Fun, Result).
retry(Max, N, _Fun, _Result) when N > Max ->
    throw(max_retries);
retry(Max, N, Fun, Result) ->
    case Fun() of
        Result ->
            Result;
        _ ->
            timer:sleep(1000),
            retry(Max, N+1, Fun, Result)
    end.
