%%%  Functions in this module might be called using RPC e.g.,
%%%  distel. To ensure that all standard output stays in 'this' node
%%%  make sure that all io:format(...) calls are made by processes in
%%%  'this' node and not via the RPC server
-module(user_action).

-behaviour(gen_server).

%% API
-export([start_link/0,
         compile/1,
         reload/1,
         compile_and_reload/1,
         test/0,
         test/2,
         test/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {changed_files = [] :: [string()],
                test}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

compile(Module) when is_atom(Module) ->
    compile(source_path(Module));


compile(Module) when is_atom(Module) ->
    compile(source_path(Module));

compile(Path) ->
    gen_server:call(?MODULE, {compile, Path}).

reload(Module) ->
    gen_server:call(?MODULE, {reload, Module}).

compile_and_reload(Module) when is_atom(Module) ->
    compile_and_reload(source_path(Module));

compile_and_reload(Path) ->
    gen_server:call(?MODULE, {compile_and_reload, Path}).

test() ->
    gen_server:call(?MODULE, test).

test(Type, Module) when is_atom(Module) ->
    test(Type, source_path(Module), _TestCase = undefined);

test(Type, Path) ->
    test(Type, Path, _TestCase = undefined).

test(Type, Module, TestCase) when is_atom(Module), is_atom(TestCase) ->
    test(Type, source_path(Module), TestCase);

test(Type, Path, TestCase) when is_atom(TestCase) ->
    gen_server:call(?MODULE, {test, {Type, Path, TestCase}}, 60*1000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({compile_and_reload, Path}, From, State) ->
    case handle_call({compile, Path}, From, State) of
        {reply, error, State1} ->
            {reply, error, State1};
        {reply, Module, State1} ->
            handle_call({reload, Module}, From, State1)
    end;

handle_call({compile, Path}, _From, State) ->
    Reply =
        try
            [Module] = compile_loop([Path]),
            maybe_update_code_path([Path]),
            Module
        catch
            throw:{error, {term, Errors}} ->
                io:format("~p~n", [Errors]),
                error;
            throw:{error, {string, Errors}} ->
                lists:foreach(fun(Error) ->
                                      io:format("~s~n", [Error])
                              end, Errors),
                error
        end,
    {reply, Reply, State};

handle_call({reload, Module}, _From, State) ->
    Reply = reload_modules([Module]),
    {reply, Reply, State};




handle_call(test, From, #state{test = {Type, Path, TestCase}} = State) ->
    handle_call({test, {Type, Path, TestCase}}, From, State);

handle_call({test, {Type, Path, TestCase}}, _From, State) ->
    Reply =
        try
            %% Get Changed files
            ChangedFiles = [Path|State#state.changed_files],
            ChangedFiles1 = fswatch_buf:flush() ++ ChangedFiles,
            ChangedFiles2 = lists:usort(ChangedFiles1),

            %% Compile all changed files
            Modules = compile_loop(ChangedFiles2),
            maybe_update_code_path(ChangedFiles2),
            ok = reload_modules(Modules),
            verbose("~n", []),
            [verbose("~-80s compiled ~n", [F]) || F <- ChangedFiles2],

            %% Report Verbose information
            [verbose("~-80s reloaded ~n", [M]) || M <- Modules],

            %% Run Test
            [TestModule|_] = Modules,
            case TestCase of
                undefined ->
                    verbose("~s ~s~n", [Type, TestModule]);
                _ ->
                    verbose("~s ~s:~s(...)~n", [Type, TestModule, TestCase])
            end,
            run_test(Type, State, TestModule, TestCase),
            ok
        catch
            throw:{error, {term, Errors}} ->
                io:format("~p~n", [Errors]),
                error;
            throw:{error, {string, Errors}} ->
                lists:foreach(fun(Error) ->
                                      io:format("~s~n", [Error])
                              end, Errors),
                error
        end,
    State1 = State#state{test = {Type, Path, TestCase}},
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
verbose(Format, Args) ->
    case application:get_env(erl_dev_tools, verbose, true) of
        true ->
            io:format(Format, Args);
        false ->
            ok
    end.

source_path(Module) ->
    proplists:get_value(source, Module:module_info(compile)).

run_test(ct, _State, Module, undefined) ->
    Opts = application:get_env(erl_dev_tools, ct_opts, []),
    Opts1 = [{auto_compile, false}] ++ Opts,
    ct:run_test(Opts1 ++ [{suite, Module}]);

run_test(ct, _State, Module, TestCase) ->
    Opts = application:get_env(erl_dev_tools, ct_opts, []),
    Opts1 = [{auto_compile, false}] ++ Opts,
    ct:run_test(Opts1 ++ [{suite, Module}, {testcase, TestCase}]);

run_test(eunit, _State, Module, undefined) ->
    Opts = [no_tty, {report, {eunit_formatter, []}}],
    eunit:test(Module, Opts);

run_test(eunit, _State, Module, TestCase) ->
    Opts = [no_tty, {report, {eunit_formatter, []}}],
    eunit:test({Module, TestCase}, Opts);

run_test(eqc, _State, Module, TestCase) when TestCase /= undefined ->
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
    [export_all, debug_info, return_errors,
     {d, 'EQC'}, {d, 'TEST'}].

parse_transform_compile_flags(Flags) ->
    case code:which(lager) of
        non_existing ->
            Flags;
        _ ->
            [{parse_transform, lager_transform}| Flags]
    end.
