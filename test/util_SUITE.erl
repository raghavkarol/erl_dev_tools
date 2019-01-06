%%%-------------------------------------------------------------------
%%% @author Raghav Karol <raghav.karol@gmail.com>
%%% @copyright (C) 2016, Raghav Karol
%%% @doc
%%%
%%% @end
%%% Created : 27 Nov 2016 by Raghav Karol <raghav.karol@gmail.com>
%%%-------------------------------------------------------------------
-module(util_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    {ok, Cwd} = file:get_cwd(),
    {ok, Home} = util:find_project_home(Cwd),
    [{home, Home} | Config].

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
    Home = ?config(home, Config),
    file:write_file(Home ++ "/test_app/src/compile_ok.erl", compile_ok_file_bin()),
    file:write_file(Home ++ "/test_app/src/compile_fail.erl", compile_fail_file_bin()),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    Home = ?config(home, Config),
    ok = file:delete(Home ++ "/test_app/src/compile_ok.erl"),
    ok = file:delete(Home ++ "/test_app/src/compile_fail.erl"),
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

%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
compile_ok_test(Config) ->
    Home = ?config(home, Config),
    CompileOkFile = Home ++ "/test_app/src/compile_ok.erl",
    {ok, compile_ok} = util:compile(CompileOkFile, []),
    ok.

compile_fail_test(Config) ->
    Home = ?config(home, Config),
    CompileFailFile = Home ++ "/test_app/src/compile_fail.erl",
    {error, [_], [_] = _Warnings} = util:compile(CompileFailFile, [return_errors]),
    ok.

compile_errors_to_emacs_parseable_string_test(Config) ->
    Home = ?config(home, Config),
    CompileFailFile = Home ++ "/test_app/src/compile_fail.erl",
    {error, Errors, _Warnings} = util:compile(CompileFailFile, [return_errors]),
    [ErrorLine] = util:compile_errors_to_emacs_parseable_string(Errors),
    [CompileFailFile, "7", " syntax error before"," ok"] =
        string:tokens(ErrorLine, ":"),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
compile_ok_file_bin() ->
    <<"-module(compile_ok).

-compile(export_all).

main() ->
  ok.
">>.

compile_fail_file_bin() ->
    <<"-module(compile_fail).

-compile(export_all).

main() ->
   alk;jasf
   ok.
">>.
