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
         find_project_home/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    OutDir = compile_opts(outdir, Path),
    filelib:ensure_dir(OutDir),
    Opts = Flags
        ++ [{i, Dir} || Dir <- compile_opts(i, Path)]
        ++ [{outdir, compile_opts(outdir, Path)}],
    compile:file(Path, Opts).

%%
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

compile_opts(outdir, Path) ->
    case application:get_env(erl_dev_tools, mode, rebar3) of
        rebar3 ->
            {ok, Home} = find_project_home(Path),
            AppName = filename:basename(Home),
            Home ++ "/_build/erl_dev_tools/lib/" ++ AppName ++ "/ebin/";
        rebar ->
            case {is_ct_suite(Path), is_in_src_dir(Path)} of
                {false, false} ->
                    filename:dirname(Path);
                {false, true} ->
                    filename:dirname(filename:dirname(Path)) ++ "/ebin";
                {true, false} ->
                    filename:dirname(Path)
            end
    end;

compile_opts(i, Path) ->
    {ok, Home} = find_project_home(Path),
    [Home ++ "/include"] ++ filelib:wildcard(Home ++ "/deps/*/include/").


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
find_project_home_test() ->
    {ok, Cwd} = file:get_cwd(),
    {ok, Cwd} = find_project_home(Cwd),
    [AppFile] = filelib:wildcard(Cwd ++ "/src/*.app.src"),
    {ok, Cwd} = find_project_home(AppFile),
    {ok, Cwd} = find_project_home(Cwd ++ "/not_a_dir/1/2/3/"),
    {ok, Cwd} = find_project_home(Cwd ++ "/not_a_dir/not_a_file"),
    {ok, "."} = find_project_home("."),
    {ok, "./."} = find_project_home("./."),
    {ok, "../erl_dev_tools"} = find_project_home("../erl_dev_tools"),

    ok.

compile_opts_rebar_test() ->
    application:set_env(erl_dev_tools, mode, rebar),
    {ok, Cwd} = file:get_cwd(),
    [ErlSrcFile|_] = filelib:wildcard(Cwd ++ "/src/*.erl"),
    [ErlTestFile|_] = filelib:wildcard(Cwd ++ "/test/*.erl"),

    ExpectedOutDirSrcFile = filename:dirname(filename:dirname(ErlSrcFile)) ++ "/ebin",
    ExpectedOutDirTestFile = filename:dirname((ErlTestFile)),

    ExpectedOutDirSrcFile = compile_opts(outdir, ErlSrcFile),
    ExpectedOutDirTestFile = compile_opts(outdir, ErlTestFile),

    ExpectedIncludeDirs = [Cwd ++ "/include"],
    ExpectedIncludeDirs = compile_opts(i, ErlSrcFile),
    ok.

compile_opts_rebar3_test() ->
    application:set_env(erl_dev_tools, mode, rebar3),
    {ok, Cwd} = file:get_cwd(),
    [ErlSrcFile|_] = filelib:wildcard(Cwd ++ "/src/*.erl"),
    [ErlTestFile|_] = filelib:wildcard(Cwd ++ "/test/*.erl"),

    ExpectedOutDir = Cwd ++ "/_build/erl_dev_tools/lib/erl_dev_tools/ebin/",
    ExpectedOutDir = compile_opts(outdir, ErlSrcFile),
    ExpectedOutDir = compile_opts(outdir, ErlTestFile),

    ExpectedIncludeDirs = [Cwd ++ "/include"],
    ExpectedIncludeDirs = compile_opts(i, ErlSrcFile),
    ok.

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
