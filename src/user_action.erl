%%%  Functions in this module might be called using RPC e.g.,
%%%  distel. To ensure that all standard output stays in 'this' node
%%%  make sure that all io:format(...) calls are made by processes in
%%%  'this' node and not via the RPC server
-module(user_action).

-behaviour(gen_server).

%% API
-export([start_link/0,
         compile/1,
         compile_all_changes/0,
         reload/1,
         watch_changes/0,
         watch_changes/1,
         compile_and_reload/1,
         compile_and_reload_all_changes/0,
         test/0,
         test/2,
         test/3,
         test/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {changed_files = [] :: [string()],
                watch_changes_tref,
                test}).

-define(TIMEOUT, trunc(timer:minutes(3))).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

compile(Module) when is_atom(Module) ->
    compile(source_path(Module));

compile(Path) ->
    gen_server:call(?SERVER, {compile, Path}).

compile_all_changes() ->
    gen_server:call(?SERVER, compile_all_changes).

reload(Module) ->
    gen_server:call(?SERVER, {reload, Module}).

compile_and_reload(Module) when is_atom(Module) ->
    compile_and_reload(source_path(Module));

compile_and_reload(Path) ->
    gen_server:call(?SERVER, {compile_and_reload, Path}).

compile_and_reload_all_changes() ->
    gen_server:call(?SERVER, compile_and_reload_all_changes).

test() ->
    gen_server:call(?SERVER, test, ?TIMEOUT).

test(Type, Module) when is_atom(Module) ->
    test(Type, source_path(Module), _TestCase = undefined);

test(Type, Path) ->
    test(Type, Path, _TestCase = undefined).

test(Type, Module, TestCase) when is_atom(Module), is_atom(TestCase) ->
    test(Type, source_path(Module), TestCase);

test(Type, Path, TestCase) when is_atom(TestCase) ->
    test(Type, Path, TestCase, _Options = []).

test(Type, Path, TestCase, Options) when is_atom(TestCase) ->
    Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
    gen_server:call(?SERVER, {test, {Type, Path, TestCase, Options}}, Timeout).

watch_changes() ->
    Interval = application:get_env(erl_dev_tools, watch_changes_interval, 5000),
    watch_changes(Interval).

watch_changes(Interval) ->
    gen_server:call(?SERVER, {watch_changes, Interval}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

maybe_cancel_timer(undefined) ->
    ok;
maybe_cancel_timer(T) ->
    timer:cancel(T).

handle_call({watch_changes, Interval}, _From, #state{watch_changes_tref = Tref} = State) ->
    maybe_cancel_timer(Tref),

    verbose("started watching changes with interval ~p msecs", [Interval]),
    {ok, Tref1} = timer:apply_interval(Interval, user_action, compile_and_reload_all_changes, []),

    State1 = State#state{watch_changes_tref = Tref1},
    Reply = ok,
    {reply, Reply, State1};

handle_call(compile_all_changes, _From, State) ->
    ChangedFiles = fswatch_buf:flush() ++ State#state.changed_files,
    Reply = compile_and_reload(ChangedFiles, _Reload = false),
    {reply, Reply, State};

handle_call(compile_and_reload_all_changes, _From, State) ->
    ChangedFiles = fswatch_buf:changed_files() ++ State#state.changed_files,
    Reply = compile_and_reload(ChangedFiles, _Reload = true),
    if Reply /= error ->
            fswatch_buf:flush();
       true ->
            ok
    end,
    {reply, Reply, State};

handle_call({compile_and_reload, Path}, _From, State) ->
    compile_and_reload([Path], _Reload = true),
    {reply, ok, State};

handle_call({compile, Path}, _From, State) ->
    Reply =
    case compile_and_reload([Path], _Reload = false) of
        error ->
            error;
        [Module] ->
            Module
    end,
    {reply, Reply, State};

handle_call({reload, Module}, _From, State) ->
    Reply = reload_modules([Module]),
    {reply, Reply, State};

handle_call(test, _From, #state{test = undefined} = State) ->
    {reply, '$no_previous_test', State};

handle_call(test, From, #state{test = TestSpec} = State) ->
    handle_call({test, TestSpec}, From, State);

handle_call({test, TestSpec = {Type, Path, TestCase, Options}}, _From, State) ->
    ChangedFiles = case filelib:is_dir(Path) of
                       true ->
                           fswatch_buf:changed_files() ++ State#state.changed_files;
                       false ->
                           [Path|fswatch_buf:changed_files() ++ State#state.changed_files]
                   end,
    Reply =
        case maybe_compile_changes(ChangedFiles, Options) of
            error ->
                {error, compilation_failed};
            Modules when is_list(Modules) ->
                _ = fswatch_buf:flush(),
                {TestDir, TestModule} = get_module(Path),
                try
                    run_test(Type, State, {TestDir, TestModule, TestCase})
                catch
                    error:undef ->
                        {error, {not_valid_testcase, TestModule, TestCase}}
                end
        end,
    State1 =
        case Reply of
            {error, _} ->
                State;
            _ ->        % EUNIT, CT and EQC all return different value
                State#state{test = TestSpec}
        end,
    {reply, Reply, State1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_compile_changes(ChangedFiles :: list(),
                            Options :: proplists:proplist()) ->
   ok
 | [atom()]
 | {error, term()}.
maybe_compile_changes(ChangedFiles, Options) ->
    NoCompile = proplists:get_value(no_compile, Options, false),
    case NoCompile of
        true ->
            ok;
        false ->
            compile_and_reload(ChangedFiles, _Reload = true)
    end.

get_module(Path) ->
    case {filelib:is_dir(Path), filelib:is_regular(Path)} of
        {true, _} ->
            {Path, undefined};
        {_, true} ->
            {filename:dirname(Path), list_to_atom(filename:basename(Path, ".erl"))}
    end.

compile_and_reload(ChangedFiles, Reload) ->
    try
        %% Compile all changed files
        ChangedFiles1 = lists:usort(ChangedFiles),
        Modules = compile_loop(ChangedFiles1),
        maybe_update_code_path(ChangedFiles1),
        verbose("erl_dev_tools~n",[]),
        if ChangedFiles1 /= [] ->
                info("~n", []);
           true ->
                ok
        end,
        [info("~-160s compiled ~n", [F]) || F <- ChangedFiles1],

        %% Report Verbose information
        case Reload of
            true ->
                ok = reload_modules(Modules),
                [info("~-160s reloaded ~n", [code:which(M)]) || M <- Modules];
            false ->
                ok
        end,
        Modules
    catch
        throw:{error, {term, Errors}} ->
            io:format("~n~p~n", [Errors]),
            error;
        throw:{error, {string, Errors}} ->
            lists:foreach(fun(Error) ->
                                  io:format("~n~s~n", [Error])
                          end, Errors),
            error
    end.


info(Format, Args) ->
    io:format("==> " ++ Format, Args).

verbose(Format, Args) ->
    case application:get_env(erl_dev_tools, verbose, true) of
        true ->
            io:format("==> " ++ Format, Args);
        false ->
            ok
    end.

source_path(Module) ->
    proplists:get_value(source, Module:module_info(compile)).

format_result({Status, Module, undefined}) ->
    {Status, Module};
format_result({Status, Module, TestCase}) ->
    {Status, Module, TestCase}.

run_test(ct, _State, {Dir, Module, TestCase}) ->
    Opts = application:get_env(erl_dev_tools, ct_opts, []),
    Opts1 = Opts
        ++ [{auto_compile, false}]
        ++ [{dir, Dir}]
        ++ [{logdir, "./_build/test/logs"}]
        ++ [{suite, Module} || Module /= undefined ]
        ++ ct_group_opts(Module, TestCase)
        ++ [{testcase, TestCase} || TestCase /= undefined ],
    Result = ct:run_test(Opts1),

    case Result of
        {_Passed, _Failed = 0, {_, _}} ->
            format_result({ok, Module, TestCase});
        {_Passed, _Failed, {_, _}} ->
            util:print_latest_suite_log(),
            format_result({error, Module, TestCase})
    end;

run_test(eunit, _State, {_Dir, Module, undefined}) ->
    Opts = [no_tty, {report, {eunit_formatter, []}}],
    eunit:test(Module, Opts);

run_test(eunit, _State, {_Dir, Module, TestCase}) ->
    Opts = [no_tty, {report, {eunit_formatter, []}}],
    eunit:test({Module, TestCase}, Opts);

run_test(eqc, _State, {_Dir, Module, TestCase}) when TestCase /= undefined ->
    eqc:quickcheck(Module:TestCase()).

reload_modules(Modules) ->
    [util:reload(Module) || Module <- Modules],
    ok.

maybe_update_code_path(Changes) ->
    CodePath = code:get_path(),
    lists:foreach(fun(Path) ->
                          EbinDir = util:compile_opts(outdir, Path),
                          case lists:member(EbinDir, CodePath) of
                              true ->
                                  ok;
                              false ->
                                  true = code:add_patha(EbinDir)
                          end
                  end, Changes).

compile_loop(Files) ->
    compile_loop([], Files).

compile_loop(Result, []) ->
    lists:reverse(Result);

compile_loop(Result, [File|Rest]) ->
    Flags = default_compile_flags(),
    Flags1 = parse_transform_compile_flags(Flags),
    NewResult =
        case util:compile(File, Flags1) of
            {ok, Module} ->
                [Module|Result];
            {error, Errors, _Warnings} ->
                StrErrors =
                    util:compile_errors_to_emacs_parseable_string(Errors),
                case StrErrors of
                    [] ->
                        throw({error, {term, Errors}});
                    _ ->
                        throw({error, {string, StrErrors}})
                    end
        end,
    compile_loop(NewResult, Rest).

default_compile_flags() ->
    [export_all, debug_info, return_errors, {d, 'TEST'}].

parse_transform_compile_flags(Flags) ->
    case code:which(lager) of
        non_existing ->
            Flags;
        _ ->
            [{parse_transform, lager_transform}| Flags]
    end.

-spec ct_group_opts(Suite :: module(), TestCase :: atom())
   -> [{group, [Group :: atom()]}] | [].
ct_group_opts(Suite, Case) ->
    Group =
        try Suite:groups() of
            Groups ->
                [ Name || {Name, _Props, Cases} <- Groups,
                          proplists:get_value(Case, Cases, false)]
        catch error:undef ->
                []
        end,
    case Group of
        [] -> Group;
        _ -> [{group, Group}]
    end.
