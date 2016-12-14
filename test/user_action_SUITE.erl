-module(user_action_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================
suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:set_env(erl_dev_tools, verbose, false),
    application:set_env(erl_dev_tools, mode, rebar3),
    application:set_env(erl_dev_tools, ct_opts, [{logdir, "/dev/null/logs"}]),
    {ok, Cwd} = file:get_cwd(),
    {ok, Home} = util:find_project_home(Cwd),
    [{home, Home},
     {test_app_home, Home ++ "/test_app/"},
     {test_app_src,  Home ++ "/test_app/src/"},
     {test_app_test,  Home ++ "/test_app/test/"},
     {test_app_ebin, Home ++ "/test_app/_build/erl_dev_tools/lib/test_app/ebin/"},
     {eunit_opts, [no_tty, {report, {eunit_formatter, []}}]},
     {ct_opts, [{logdir, "/dev/null/logs"},
                {refresh_logs, "/dev/null/logs"},
                {auto_compile, false}]}
     | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    meck:unload(),
    EbinDir = ?config(test_app_ebin, Config),
    os:cmd("rm -rf " ++ EbinDir),
    os:putenv("WATCH_DIRS", ?config(test_app_home, Config)),

    ok = application:start(erl_dev_tools),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = application:stop(erl_dev_tools),
    undefined = fswatch_SUITE:retry(fun() -> whereis(fswatch_sup) end,
                                    undefined),
    ok.

all() ->
    util:all_tests(?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================
code_path_updated_test(Config) ->
    SrcDir = ?config(test_app_src, Config),
    CompileOutDir = util:compile_opts(outdir, SrcDir ++ "/file1.erl"),
    ok = filelib:ensure_dir(CompileOutDir),
    code:del_path(maybe_remove_trailing_slash(CompileOutDir)),
    file3 = user_action:compile(SrcDir ++ "/file3.erl"),
    true = lists:member(maybe_remove_trailing_slash(CompileOutDir), code:get_path()),
    ok.

compile_test(Config) ->
    SrcDir = ?config(test_app_src, Config),
    EbinDir = ?config(test_app_ebin, Config),
    false = filelib:is_regular(EbinDir ++ "/file1.beam"),
    file1 = user_action:compile(SrcDir ++ "/file1.erl"),
    true = filelib:is_regular(EbinDir ++ "/file1.beam"),
    ok.

reload_test(Config) ->
    SrcDir = ?config(test_app_src, Config),
    EbinDir = ?config(test_app_ebin, Config),
    code:purge(file1),
    file1 = user_action:compile(SrcDir ++ "/file1.erl"),
    ok = user_action:reload(file1),
    Expected = EbinDir ++ "file1.beam",
    {file, Expected} = code:is_loaded(file1),
    ok.

compile_and_reload_test(Config) ->
    SrcDir = ?config(test_app_src, Config),
    EbinDir = ?config(test_app_ebin, Config),
    code:purge(file1),
    ok = user_action:compile_and_reload(SrcDir ++ "/file1.erl"),
    true = filelib:is_regular(EbinDir ++ "/file1.beam"),
    Expected = EbinDir ++ "file1.beam",
    {file, Expected} = code:is_loaded(file1),
    ok.

compile_and_reload_all_changed_test(Config) ->
    SrcDir = ?config(test_app_src, Config),
    EbinDir = ?config(test_app_ebin, Config),
    code:delete(file1),
    code:delete(file2),
    os:cmd("touch " ++ SrcDir ++ "/file1.erl"),
    os:cmd("touch " ++ SrcDir ++ "/file2.erl"),
    [_, _] = fswatch_buf:wait_get_changes(),
    Result = user_action:compile_and_reload_all_changes(),
    [file1, file2] = lists:sort(Result),
    true = filelib:is_regular(EbinDir ++ "/file1.beam"),
    true = filelib:is_regular(EbinDir ++ "/file2.beam"),
    Expected1 = EbinDir ++ "file1.beam",
    {file, Expected1} = code:is_loaded(file1),
    Expected2 = EbinDir ++ "file2.beam",
    {file, Expected2} = code:is_loaded(file2),
    ok.

compile_all_changed_test(Config) ->
    SrcDir = ?config(test_app_src, Config),
    EbinDir = ?config(test_app_ebin, Config),
    code:purge(file1),
    code:delete(file1),
    code:purge(file2),
    code:delete(file2),
    os:cmd("touch " ++ SrcDir ++ "/file1.erl"),
    os:cmd("touch " ++ SrcDir ++ "/file2.erl"),
    [_, _] = fswatch_buf:wait_get_changes(),
    Result = user_action:compile_all_changes(),
    [file1, file2] = lists:sort(Result),
    true = filelib:is_regular(EbinDir ++ "/file1.beam"),
    true = filelib:is_regular(EbinDir ++ "/file2.beam"),
    false = code:is_loaded(file1),
    false = code:is_loaded(file2),
    ok.

eunit_path_test(Config) ->
    Opts = ?config(eunit_opts, Config),
    TestDir = ?config(test_app_test, Config),
    TestFile = TestDir ++ "file1_test.erl",

    ok = meck:new(eunit),
    ok = meck:expect(eunit, test, fun meck_eunit/2),
    ok = user_action:test(eunit, TestFile),
    true = meck:called(eunit, test, [file1_test, Opts]),

    ok = user_action:test(eunit, TestFile, dummy_test),
    true = meck:called(eunit, test, [{file1_test, dummy_test}, Opts]),

    ok.

eunit_module_test(Config) ->
    Opts = ?config(eunit_opts, Config),
    TestDir = ?config(test_app_test, Config),
    TestFile = TestDir ++ "file1_test.erl",
    file1_test = user_action:compile(TestFile),

    ok = meck:new(eunit),
    ok = meck:expect(eunit, test, fun meck_eunit/2),
    ok = user_action:test(eunit, file1_test),
    true = meck:called(eunit, test, [file1_test, Opts]),

    ok = user_action:test(eunit, file1_test, dummy_test),
    true = meck:called(eunit, test, [{file1_test, dummy_test}, Opts]),
    ok.

ct_path_test(Config) ->
    Opts = ?config(ct_opts, Config),
    TestDir = ?config(test_app_test, Config),
    TestFile = TestDir ++ "file1_SUITE.erl",

    ok = meck:new(ct),
    ok = meck:expect(ct, run_test, fun meck_ct_run/1),
    ok = user_action:test(ct, TestFile),
    true = meck:called(ct, run_test, [Opts ++ [{suite,file1_SUITE}]]),

    ok = user_action:test(ct, TestFile, dummy_test),
    true = meck:called(ct, run_test, [Opts ++ [{suite,file1_SUITE},{testcase, dummy_test}]]),
    ok.

ct_module_test(Config) ->
    Opts = ?config(ct_opts, Config),
    TestDir = ?config(test_app_test, Config),
    file1_SUITE = user_action:compile(TestDir ++ "file1_SUITE.erl"),
    file1_with_groups_SUITE = user_action:compile(TestDir ++ "file1_with_groups_SUITE.erl"),

    ok = meck:new(ct),
    ok = meck:expect(ct, run_test, fun meck_ct_run/1),
    ok = user_action:test(ct, file1_SUITE),
    ct:pal("meck:history(ct): ~p", [meck:history(ct)]),
    ct:pal("Opts: ~p", [Opts]),
    true = meck:called(ct, run_test, [Opts ++ [{suite,file1_SUITE}]]),

    ok = user_action:test(ct, file1_SUITE, dummy_test),
    true = meck:called(ct, run_test, [Opts ++ [{suite,file1_SUITE}, {testcase, dummy_test}]]),

    ok = user_action:test(ct, file1_with_groups_SUITE, dummy_test),
    true = meck:called(ct, run_test, [Opts ++ [{suite,file1_with_groups_SUITE}, {testcase, dummy_test}]]),

    ok = user_action:test(ct, file1_with_groups_SUITE, group_dummy_test),
    true = meck:called(ct, run_test, [Opts ++ [{suite,file1_with_groups_SUITE},
                                               {group, [group1]},
                                               {testcase, group_dummy_test}]]),

    ok.

eqc_path_test(Config) ->
    TestDir = ?config(test_app_test, Config),
    TestFile = TestDir ++ "file1_eqc.erl",

    ok = meck:new(eqc),
    ok = meck:expect(eqc, quickcheck, fun meck_ct_eqc_quickcheck/1),
    user_action:test(eqc, TestFile, prop_dummy),
    true = meck:called(eqc, quickcheck, [file1_eqc:prop_dummy()]),
    ok.

eqc_module_test(Config) ->
    TestDir = ?config(test_app_test, Config),
    TestFile = TestDir ++ "file1_eqc.erl",
    file1_eqc = user_action:compile(TestFile),

    ok = meck:new(eqc),
    ok = meck:expect(eqc, quickcheck, fun meck_ct_eqc_quickcheck/1),
    user_action:test(eqc, file1_eqc, prop_dummy),
    true = meck:called(eqc, quickcheck, [file1_eqc:prop_dummy()]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
meck_eunit({_Module, _TestCase}, _Opts) ->
    ok;
meck_eunit(_Module, _Opts) ->
    ok.

meck_ct_run(_Props) ->
    ok.

meck_ct_eqc_quickcheck(_Prop) ->
    ok.

maybe_remove_trailing_slash(Path) ->
    case lists:reverse(Path) of
        [$/|Rest] ->
            lists:reverse(Rest);
        _ ->
            Path
    end.
