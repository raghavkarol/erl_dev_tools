-module(util).

-export([all_tests/1,
         is_testcase/1,
         getenv/2,
         get_watch_dirs/1,
         compile/2,
         compile_and_reload/1,
         compile_and_reload/2,
         compile_errors_to_emacs_parseable_string/1,
         compile_opts/2,
         reload/1,
         find_project_home/1,
         is_ct_suite/1,
         is_in_src_dir/1,
         latest_suite_log/0,
         print_suite_log/1,
         print_latest_suite_log/0
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

prefix_path(Prefix, Path) ->
    io_lib:format("~s/~s", [Prefix, Path]).

suite_log_pattern() ->
    "_build/test/logs/**/suite.log".

latest_file(Pattern) ->
    Dirs1 = filelib:wildcard(Pattern),
    Dirs2 = lists:sort(Dirs1),
    Dirs3 = lists:reverse(Dirs2),

    [Latest| _ ] = Dirs3,
    Latest.

latest_suite_log() ->
    {ok, Cwd} = file:get_cwd(),
    Pattern = prefix_path(Cwd, suite_log_pattern()),
    File = latest_file(Pattern),
    File.

print_suite_log(File) ->
    Header =  "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -",
    {ok, Contents} = file:read_file(File),
    Lines = binary:split(Contents, <<"\n">>, [global]),
    io:format("~s~n", [Header]),
    lists:foreach(fun(_Line = <<"=result        ", Rest/binary>>) ->
                          io:format("~s~n", [Rest]);
                     (Line = <<"   ", _/binary>>) ->
                          io:format("~s~n", [Line]);
                     (_Line) ->
                          ignore
                  end,
                  Lines),
    io:format("~s~n", [Header]),
    ok.

print_latest_suite_log() ->
    File = latest_suite_log(),
    print_suite_log(File).


%% Could normalize path using a shell command
%% os:cmd("cd /Users/raghav/github/erl_dev_tools/../erl_dev_tools/../erl_dev_tools/ ; pwd").

all_tests(Module) ->
    [F || {F, A} <- Module:module_info(exports), is_testcase({F, A})].

is_testcase({F, 1}) ->
    match =:= re:run(atom_to_list(F), "_test$", [{capture, none}]);
is_testcase({_, _}) ->
    false.

getenv(VarName, Default) ->
    case os:getenv(VarName) of
        false ->
            Default;
        Value ->
            Value
    end.

get_watch_dirs(WatchDirsStr) ->
    string:tokens(WatchDirsStr, ":").

compile_and_reload(Path) ->
    compile_and_reload(Path, _Flags = []).

compile_and_reload(Path, Flags) ->
    case compile(Path, Flags) of
        {ok, Module} ->
            reload(Module),
            ok;
        {error, Errors, Warnings} ->
            {error, Path, {Errors, Warnings}}
    end.

compile(Path, Flags) ->
    Opts = Flags
        ++ [{i, Dir} || Dir <- compile_opts(i, Path)]
        ++ [{outdir, outdir(Path)}],

    compile:file(Path, Opts).

is_test(Path) ->
    [[]|Words] = string:split(Path, "/", all),
    lists:member("test", Words).

is_checkouts(Path) ->
    [[]|Words] = string:split(Path, "/", all),
    lists:member("_checkouts", Words).

outdir(Path) ->
    Home = project_home(),
    App = app_name(Path),
    case is_test(Path) of
        true ->
            filename:dirname(Path);
        false ->
            case is_checkouts(Path) of
                true ->
                    Home ++ "/" ++ "_checkouts" ++ "/" ++ App ++ "/ebin/";
                false ->
                             Profile = profile_name(Path),
                    Home ++ "/" ++ "_build" ++ "/" ++ Profile ++ "/lib/" ++ App ++ "/ebin/"
            end
    end.


project_home() ->
    {ok, DefaultHome} = file:get_cwd(),
    DefaultHome1 = application:get_env(erl_dev_tools, project_home, DefaultHome),
    os:getenv("ERL_DEV_TOOLS_PROJECT_HOME", DefaultHome1).

drop_until_word(W, [W|Rest]) ->
    [W|Rest];
drop_until_word(W, [_|Rest]) ->
    drop_until_word(W, Rest);
drop_until_word(_W, []) ->
    [].

app_name1([App, "src"|_]) ->
    App;
app_name1([App, "test"|_]) ->
    App;
app_name1([_, _|Rest]) ->
    app_name1(Rest);
app_name1([_]) ->
    undefined;
app_name1([]) ->
    undefined.


app_name(Path) ->
    [[]|Words] = string:split(Path, "/", all),
    app_name1(Words).

profile_name(Path) ->
    [[]|Words] = string:split(Path, "/", all),
    Words1 = drop_until_word("_build", Words),
    case Words1 of
        [] ->
            case lists:reverse(Words) of
                [_Filename, "src"|_] ->
                    "default";
                [_Filename, "test"|_] ->
                    "test"
            end;
        ["_build", RebarProfile|_] ->
            RebarProfile
    end.


%% [{"/Users/raghav/github/erl_dev_tools/test_data/compile_fail.erl",
%%           [{5,erl_parse,["syntax error before: ",[]]}]}]
compile_errors_to_emacs_parseable_string(Errors) ->
    [ begin
          Line =
              case Rest of
                  [] ->
                      io_lib:format("~s:~p: ~s",
                                    [Path, LineNo, ErrorMsg]);
                  Rest ->
                      io_lib:format("~s:~p: ~s~s",
                                    [Path, LineNo, ErrorMsg, Rest])
              end,
          lists:flatten(Line)
      end || {Path, [{LineNo, erl_parse, [ErrorMsg, Rest]}]} <- Errors].

reload(Module) ->
    code:purge(Module),
    code:load_file(Module).


find_project_home("/") ->
    {error, not_found};
find_project_home(Path) ->
    Dir =
        case filelib:is_dir(Path) of
            true ->
                Path;
            false ->
                filename:dirname(Path)
        end,
    case filelib:is_file(Dir ++ "/rebar.config") of
        true ->
            {ok, Dir};
        false ->
            find_project_home(filename:dirname(Dir))
    end.

%% -include_lib("erlcloud/include/erlcloud_aws.hrl").
%% -include_lib("erlcloud_aws.hrl").
compile_opts(outdir, Path) ->
    outdir(Path);

compile_opts(i, Path) ->
    {ok, Home} = find_project_home(Path),
    IncludeDirs = [Home ++ "/include"] ++
        filelib:wildcard(Home ++ "/_build/default/lib/*/include"),
    IncludeLibDirs = filelib:wildcard(Home ++ "/_checkouts") ++
        filelib:wildcard(Home ++ "/_build/default/lib"),
    IncludeLibDirs ++ IncludeDirs.

is_ct_suite(Path) ->
    case lists:reverse(filename:basename(Path, ".erl")) of
        "ETIUS_" ++ _ -> true;
        _ -> false
    end.

is_in_src_dir(Path) ->
    case lists:reverse(filename:dirname(Path)) of
        "crs/" ++ _ -> true;
        "./crs/" ++ _ -> true;
        _ -> false
    end.

-ifdef(TEST).
app_name_test() ->
    ?assertEqual(
       "aefr_eng",
       app_name("/Users/rkarol-admin/github/alertlogic/aefr_eng/src/aefr_eng.erl")),

    ?assertEqual(
       "aefr_eng",
       app_name("/Users/rkarol-admin/github/alertlogic/aefr_eng/test/aefr_eng_suite.erl")),
    ok.

profile_name_test() ->
    ?assertEqual(
       "test",
       profile_name("/Users/rkarol-admin/github/alertlogic/aefr_eng/test/aefr_eng_SUITE.erl")),

    ?assertEqual(
       "default",
       profile_name("/Users/rkarol-admin/github/alertlogic/aefr_eng/src/aefr_eng.erl")),
    ?assertEqual(
       "default",
       profile_name("/Users/rkarol-admin/github/alertlogic/aefr_eng/_build/default/lib/al_sd2/src/al_sd2.erl")),

    ?assertEqual(
       "eqc",
       profile_name("/Users/rkarol-admin/github/alertlogic/aefr_eng/_build/eqc/lib/al_sd2/src/al_sd2.erl")),
    ?assertEqual(
       "test",
       profile_name("/Users/rkarol-admin/github/alertlogic/aefr_eng/_build/test/lib/meck/src/meck.erl")),
    ok.

outdir_test() ->
    ?assertEqual(
       "/Users/rkarol-admin/github/alertlogic/aefr_eng/_build/default/lib/aefr_eng/ebin/",
       outdir("/Users/rkarol-admin/github/alertlogic/aefr_eng/src/aefr_eng.erl")),

    ?assertEqual(
       "/Users/rkarol-admin/github/alertlogic/aefr_eng/_build/test/lib/aefr_eng/ebin/",
       outdir("/Users/rkarol-admin/github/alertlogic/aefr_eng/test/aefr_eng_SUITE.erl")),

    ?assertEqual(
       "/Users/rkarol-admin/github/alertlogic/aefr_eng/_build/default/lib/al_sd2/ebin/",
       outdir("/Users/rkarol-admin/github/alertlogic/aefr_eng/_build/default/lib/al_sd2/src/al_sd2.erl")),
    ok.

%% compile_opts_rebar3_test() ->
%%     application:set_env(erl_dev_tools, mode, rebar3),
%%     {ok, Cwd} = file:get_cwd(),
%%     [ErlSrcFile|_] = filelib:wildcard(Cwd ++ "/src/*.erl"),
%%     [ErlTestFile|_] = filelib:wildcard(Cwd ++ "/test/*.erl"),
%%     ExpectedOutDir = Cwd ++ "/_build/erl_dev_tools/lib/erl_dev_tools/ebin/",
%%     ExpectedOutDir = compile_opts(outdir, ErlSrcFile),
%%     ExpectedOutDir = compile_opts(outdir, ErlTestFile),

%%     ExpectedIncludeDirs = [Cwd ++ "/_build/default/lib",
%%                            Cwd ++ "/include",
%%                            Cwd ++ "/_build/default/lib/erl_dev_tools/include"],
%%     ExpectedIncludeDirs = compile_opts(i, ErlSrcFile),
%%     ok.

is_ct_suite_test() ->
    false = is_ct_suite("/a/b/c/x.erl"),
    false = is_ct_suite("/a/b/c/x_SUITE.beam"),
    true = is_ct_suite("/a/b/c/x_SUITE.erl"),
    true = is_ct_suite("x_SUITE.erl"),
    ok.

is_in_src_dir_test() ->
    false = is_in_src_dir("/a/b/c/x.erl"),
    true = is_in_src_dir("/a/b/c/src/x.erl"),
    true = is_in_src_dir("/a/b/c/src/./x.erl"),
    ok.

-endif.
