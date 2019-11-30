%%% The poor man's profiler, profiles on wall clock time.
-module(cprof).

-behaviour(gen_statem).                         %

%% API
-export([
         start_link/0,
         start_link/1,
         start/0,
         start/1,
         stop/0
        ]).

%% API
-export([
         add/1,
         add/3,
         stop_tracer/0,
         pretty_print/1,
         top_calls/2,
         reset/0
        ]).

%% tracer callback
-export([trace/2]).

%% gen_server callbacks
-export([init/1, callback_mode/0, handle_event/4,
         terminate/3, code_change/3]).

-define(SERVER, ?MODULE).
-define(PROFILE_TABLE, cprof_result).
-define(PROCESS_TABLE, cprof_processes).

-record(state, {max_calls = 100, num_calls = 0}).

-record(call, {key, module, function, time=0}).

-record(process, {pid=undefined, stack=[]}).

-record(profile, {key, module, function, count=0, total_time=0}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec add(Mod::atom()) -> ok.
add(Mod) ->
    dbg:tpl(Mod, [{'_', [], [{return_trace}]}]).

-spec add(Mod::atom(), Fun::atom(), Arity::pos_integer()) -> ok.
add(Mod, Fun, Arity) ->
    dbg:tpl(Mod, Fun, Arity, [{'_', [], [{return_trace}]}]).

pretty_print(Calls) ->
    io:format("~s~n", [[with_width("Module:Function", -20),
                        " ",
                        with_width("Total Time (msecs)", 20),
                        " ",
                        with_width("Count", 10)]]),
    io:format("~s~n", [[lists:duplicate(20, "-"),
                        " ",
                        lists:duplicate(20, "-"),
                        " ",
                        lists:duplicate(10, "-")]]),
    io:format("~s~n", [[[with_width([to_string(M), ":", to_string(F)], -20),
                         " ",
                         with_width(to_string(TotalTime), 20),
                         " ",
                         with_width(to_string(Count), 10),
                         "\n"]
                        || #profile{module=M,
                                    function=F,
                                    count=Count,
                                    total_time=TotalTime} <- Calls]]).


%% -----------------------------------------------------------------------------
%% Server control API
%% -----------------------------------------------------------------------------
start_link() ->
    start_link(10).

start_link(MaxCalls) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [MaxCalls], []).

start() ->
    start(10).

start(MaxCalls) ->
    gen_statem:start({local, ?SERVER}, ?MODULE, [MaxCalls], []).

stop() ->
    gen_statem:stop(?SERVER).

top_calls(Type, N) when Type == total_time;
                        Type == count ->
    gen_statem:call(?SERVER, {top_calls, Type, N}).

reset() ->
    gen_statem:call(?SERVER, reset).

%% -----------------------------------------------------------------------------
%% tracer callback
%%
%% Msg: {trace, Pid, return_from, {Mod, Fun, Args}, _Result}
%%      {trace, Pid, call, {Mod, Fun, _Args}}
%% -----------------------------------------------------------------------------
trace(Msg, _State) ->
    gen_statem:call(?SERVER, Msg).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------
callback_mode() ->
    handle_event_function.

init([MaxCalls]) ->
    process_flag(trap_exit, true),

    init1(),

    {ok, ready, new_state(MaxCalls)}.

handle_event({call, From}, {trace, _, call, _}, limit, _Data) ->
    Actions = [tracer_reply(From)],
    {keep_state_and_data, Actions};

handle_event({call, From}, {trace, _, return_from, _, _}, limit, _Data) ->
    Actions = [tracer_reply(From)],
    {keep_state_and_data, Actions};

handle_event({call, From}, {trace, _, call, _} = Trace, ready, Data) ->
    {trace, Pid, call, MFArgs} = Trace,
    {M, F, _A} = MFArgs,
    Key = {M, F},
    Call = #call{key=Key, module=M, function=F, time=current_time()},
    Process =
        case ets:lookup(?PROCESS_TABLE, Pid) of
            [] ->
                #process{pid=Pid};
            [P=#process{}] ->
                P
        end,
    #process{stack = Stack} = Process,
    Process1 = Process#process{stack = [Call|Stack]},
    Actions = [tracer_reply(From)],
    ets:insert(?PROCESS_TABLE, Process1),
    {keep_state, Data, Actions};

handle_event({call, From}, {trace, _, return_from, _, _} = Trace, ready, Data) ->
    {trace, Pid, return_from, MFArity, _Result} = Trace,
    {M, F, _Arity} = MFArity,
    Key = {M, F},
    Profile =
        case ets:lookup(?PROFILE_TABLE, Key) of
            [] ->
                #profile{key=Key, module=M, function=F};
            [P0 = #profile{}] ->
                P0
        end,
    Process =
        case ets:lookup(?PROCESS_TABLE, Pid) of
            [P1 = #process{}] ->
                P1
        end,
    Now = current_time(),
    #process{stack=[Top|Rest]} = Process,
    #call{time=Time} = Top,
    #profile{total_time=TotalTime, count=Count} = Profile, % TODO: ensure that we match the Module and function in profile and top of the stack
    Profile1 = Profile#profile{total_time=TotalTime+(Now-Time), count=Count+1},
    Process1 = Process#process{stack=Rest},

    ets:insert(?PROFILE_TABLE, Profile1),
    ets:insert(?PROCESS_TABLE, Process1),

    Actions = [tracer_reply(From)],
    Data1 = update_state(Data),
    maybe_stop_tracer(Data1),
    case is_tracer_limit(Data1) of
        true ->
            io:format("tracer limit reached.~n", []),
            {next_state, limit, Data1, Actions};
        false ->
            {keep_state, Data1, Actions}
    end;

handle_event({call, From}, reset, _State, Data) ->
    delete_storage(),
    init1(),
    Data1 = new_state(Data#state.max_calls),
    {next_state, ready, Data1, [{reply, From, ok}]};

handle_event({call, From}, {top_calls, Type, N}, _State, Data) ->
    Result = ets:tab2list(?PROFILE_TABLE),
    Keypos = case Type of
                 total_time ->
                     #profile.total_time;
                 count ->
                     #profile.count
             end,

    Result1 = top(N, Keypos, Result),
    {keep_state, Data, [{reply, From, Result1}]};


handle_event({call, From}, Event, State, _Data) ->
    io:format("unexpected event: ~p in State: ~p", [Event, State]),
    {keep_state_and_data, [{reply, From, ok}]}.


terminate(_Reason, _State, _Data) ->
    stop_tracer(),
    delete_storage(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

%% Initializization
init1() ->
    create_storage(),
    start_tracer().

create_storage() ->
    ?PROFILE_TABLE = ets:new(?PROFILE_TABLE, [named_table, {keypos, #profile.key}]),
    ?PROCESS_TABLE = ets:new(?PROCESS_TABLE, [named_table, {keypos, #process.pid}]).

delete_storage() ->
    true = ets:delete(?PROFILE_TABLE),
    true = ets:delete(?PROCESS_TABLE).

start_tracer() ->
    dbg:tracer(process, {fun ?MODULE:trace/2, trace_state}),
    dbg:p(all, call).

stop_tracer() ->
    dbg:ctp(),
    dbg:stop_clear().

%% Process state update functions
update_state(#state{num_calls = Num} = State) ->
    State#state{num_calls = Num + 1}.

new_state(MaxCalls) ->
    #state{max_calls = MaxCalls}.

is_tracer_limit(#state{num_calls = Num, max_calls = Max}) ->
    Num >= Max.

%% Utility
tracer_reply(From) ->
    {reply, From, trace_state}.

top(N, Keypos, TupleList) ->
    L1 = lists:keysort(Keypos, TupleList),
    L2 = lists:reverse(L1),
    lists:sublist(L2, N).

current_time() ->
    erlang:system_time(millisecond).

maybe_stop_tracer(#state{num_calls = N, max_calls = Max}) when N >= Max ->
    stop_tracer();

maybe_stop_tracer(_) ->
    ok.

to_string(A) when is_atom(A) ->
    atom_to_list(A);

to_string(N) when is_integer(N) ->
    integer_to_list(N);

to_string(L) ->
    L.

with_width(T, W) ->
    lists:flatten(io_lib:format("~" ++ to_string(W) ++ "s", [to_string(T)])).
