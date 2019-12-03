%%% The poor man's profiler, profiles on wall clock time.
-module(cprof).

-behaviour(gen_statem).

%% API
-export([start_link/0,
         start_link/1,
         start/0,
         start/1,
         stop/0]).

%% API
-export([add/1,
         add/3,
         enable_capture_calls/2,
         pretty_print/1,
         pretty_print/2,
         pretty_print_slow_call/1,
         pretty_print_slow_call/2,
         slow_call_by_id/1,
         reset/0,
         reset/1,
         stop_tracer/0,
         top_calls/2]).

%% tracer callback
-export([trace/2]).

%% gen_server callbacks
-export([init/1, callback_mode/0, handle_event/4,
         terminate/3, code_change/3]).

-define(DEFAULT_MAX_CALLS, 10).
-define(INFINITY, trunc(8.64e7)).

-define(SERVER, ?MODULE).
-define(PROFILE_TABLE, cprof_result).
-define(PROCESS_TABLE, cprof_processes).
-define(SLOW_CALL_TABLE, cprof_slow_calls).

-record(state, {max_calls=100,
                num_calls=0,
                max_capture_calls=10,           % rename max_capture_calls and slow_call_threshold
                slow_call_threshold=?INFINITY}).

-record(call, {key, module, function, time=0, args=undefined}).

-record(process, {pid=undefined, stack=[]}).

-record(profile, {key, module, function, count=0, total_time=0, times=[]}).

-record(slow_call, {key, module, function, args, time}).

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
    pretty_print(calls, Calls).

pretty_print_slow_call(Call) ->
    pretty_print_slow_call(Call, 128).

pretty_print_slow_call({ok, S}, Width) ->
    #slow_call{time=Time, module=M, function=F, args=Args} = S,
    io:format("~n", []),
    io:format("pretty printing using erlang apply~n", []),
    io:format("----------------------------------~n", []),
    io:format("erlang:apply(~p, ~p,~n~160P)~n", [M, F, Args, Width]),
    io:format("took time ~p ms~n", [Time]);

pretty_print_slow_call({error, E}, _) ->
    io:format("~p", [E]).

pretty_print(slow_calls, Calls) ->
    io:format("~s~n", [[with_width("Key", -15),
                        " ",
                        with_width("Module:Function", 20),
                        " ",
                        with_width("Time (ms)", 10),
                        " ",
                        with_width("Args", -80),
                        " "]]),
    io:format("~s~n", [[lists:duplicate(15, "-"),
                        " ",
                        lists:duplicate(20, "-"),
                        " ",
                        lists:duplicate(10, "-"),
                        " ",
                        lists:duplicate(80, "-"),
                        " "]]),
    io:format("~s~n", [[[with_width(to_string(Key), -15),
                         " ",
                         with_width([to_string(M), ":", to_string(F)], 20),
                         " ",
                         with_width(to_string(Time), 10),
                         " ",
                         with_width(io_lib:format("~W", [Args, 77]), -80),
                         " ",
                         "\n"]
                        || #slow_call{key=Key,
                                      module=M,
                                      function=F,
                                      args=Args,
                                      time=Time} <- Calls]]);

pretty_print(calls, Calls) ->
    Percentiles = [0.99, 0.75, 0.50],
    io:format("~s~n", [[with_width("Module:Function", -20),
                        " ",
                        with_width("Total time ms", 20),
                        " ",
                        with_width("Count", 10),
                        " ",
                        [[with_width(["p", to_string(trunc(P*100)), "(ms)"], 10), " "] || P <- Percentiles]
                       ]]),
    io:format("~s~n", [[lists:duplicate(20, "-"),
                        " ",
                        lists:duplicate(20, "-"),
                        " ",
                        lists:duplicate(10, "-"),
                        " ",
                        [[lists:duplicate(10, "-"), " "] || _P <- Percentiles]
                       ]]),
    io:format("~s~n", [[[with_width([to_string(M), ":", to_string(F)], -20),
                         " ",
                         with_width(to_string(TotalTime), 20),
                         " ",
                         with_width(to_string(Count), 10),
                         " ",
                         [[with_width(to_string(T), 10), " "] || {_P, T} <- percentiles(Times, Percentiles)],
                         "\n"]
                        || #profile{module=M,
                                    function=F,
                                    count=Count,
                                    total_time=TotalTime,
                                    times = Times} <- Calls]]).


%% -----------------------------------------------------------------------------
%% Server control API
%% -----------------------------------------------------------------------------
start_link() ->
    start_link(?DEFAULT_MAX_CALLS).

start_link(MaxCalls) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [MaxCalls], []).

start() ->
    start(?DEFAULT_MAX_CALLS).

start(MaxCalls) ->
    gen_statem:start({local, ?SERVER}, ?MODULE, [MaxCalls], []).

reset() ->
    reset(?DEFAULT_MAX_CALLS).

reset(MaxCalls) ->
    gen_statem:call(?SERVER, {reset, MaxCalls}).

stop() ->
    gen_statem:stop(?SERVER).

slow_call_by_id(Id) when is_integer(Id) ->
    gen_statem:call(?SERVER, {slow_call_by_id, Id}).

top_calls(Type, N) when Type == slow_calls;
                        Type == total_time;
                        Type == count ->
    gen_statem:call(?SERVER, {top_calls, Type, N}).

enable_capture_calls(MaxCalls, ThresholdMs) when MaxCalls =< 100 ->
    gen_statem:call(?SERVER, {enable_capture_calls, MaxCalls, ThresholdMs}).

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

handle_event({call, From}, {reset, MaxCalls}, _State, _Data) ->
    delete_storage(),
    init1(),
    {next_state, ready, new_state(MaxCalls), [{reply, From, ok}]};

handle_event({call, From}, {enable_capture_calls, MaxCalls, ThresholdMs}, _State, Data) ->
    Data1 = Data#state{slow_call_threshold = ThresholdMs,
                       max_capture_calls=MaxCalls},
    {keep_state, Data1, [{reply, From, ok}]};

handle_event({call, From}, {trace, _, call, _}, limit, _Data) ->
    Actions = [tracer_reply(From)],
    {keep_state_and_data, Actions};

handle_event({call, From}, {trace, _, return_from, _, _}, limit, _Data) ->
    Actions = [tracer_reply(From)],
    {keep_state_and_data, Actions};

handle_event({call, From}, {trace, _, call, _} = Trace, ready, Data) ->
    {trace, Pid, call, MFArgs} = Trace,
    {M, F, Args} = MFArgs,
    Key = {M, F},
    Call = #call{key=Key, module=M, function=F, time=current_time(), args=Args},
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
    #call{module=M, function=F, time=Time, args=Args} = Top,
    #profile{total_time=TotalTime, count=Count, times=Times} = Profile,
    Elapsed = (Now-Time),
    Profile1 = Profile#profile{total_time=Elapsed+TotalTime, count=Count+1, times=[Elapsed|Times]},
    Process1 = Process#process{stack=Rest},

    SlowCallTableSize = ets:info(?SLOW_CALL_TABLE, size),
    case SlowCallTableSize < Data#state.max_capture_calls andalso Elapsed >= Data#state.slow_call_threshold of
        true ->
            SlowCallKey = erlang:phash2(make_ref()),
            SlowCall = #slow_call{key=SlowCallKey, module=M, function=F, time=Elapsed, args=Args},
            ets:insert(?SLOW_CALL_TABLE, SlowCall);
        _ ->
            ok
    end,
    ets:insert(?PROFILE_TABLE, Profile1),
    ets:insert(?PROCESS_TABLE, Process1),

    Actions = [tracer_reply(From)],
    Data1 = update_state(Data),
    maybe_stop_tracer(Data1),
    case is_tracer_limit(Data1) of
        true ->
            io:format("tracer limit ~p reached ~n", [Data#state.max_calls]),
            {next_state, limit, Data1, Actions};
        false ->
            {keep_state, Data1, Actions}
    end;

handle_event({call, From}, {slow_call_by_id, Id}, _State, _Data) ->
    Result1 = case ets:lookup(?SLOW_CALL_TABLE, Id) of
                  [Call] ->
                      {ok, Call};
                  [] ->
                      {error, not_found}
              end,
    {keep_state_and_data, [{reply, From, Result1}]};

handle_event({call, From}, {top_calls, slow_calls, N}, _State, _Data) ->
    Result = ets:tab2list(?SLOW_CALL_TABLE),
    Result1 = top(N, #slow_call.time, Result),
    {keep_state_and_data, [{reply, From, Result1}]};

handle_event({call, From}, {top_calls, total_time, N}, _State, _Data) ->
    Result = ets:tab2list(?PROFILE_TABLE),
    Result1 = top(N, #profile.total_time, Result),
    {keep_state_and_data, [{reply, From, Result1}]};

handle_event({call, From}, {top_calls, count, N}, _State, _Data) ->
    Result = ets:tab2list(?PROFILE_TABLE),
    Result1 = top(N, #profile.count, Result),
    {keep_state_and_data, [{reply, From, Result1}]};

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
    ?PROCESS_TABLE = ets:new(?PROCESS_TABLE, [named_table, {keypos, #process.pid}]),
    ?SLOW_CALL_TABLE = ets:new(?SLOW_CALL_TABLE, [named_table, {keypos, #slow_call.key}]).

delete_storage() ->
    true = ets:delete(?PROFILE_TABLE),
    true = ets:delete(?PROCESS_TABLE),
    true = ets:delete(?SLOW_CALL_TABLE).


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

%% https://gist.github.com/ferd/6008781
percentiles(Numbers) ->
    %% [0.50, 0.75, 0.90, 0.95, 0.99, 0.999]
    percentiles(Numbers, [0.99, 0.95, 0.50]).

percentiles(Numbers, P) ->
    Percentile = fun(List, Size, Perc) ->
        Element = round(Perc * Size),
        lists:nth(Element, List)
    end,
    Len = length(Numbers),
    Sorted = lists:sort(Numbers),
    [{trunc(Perc*100), Percentile(Sorted, Len, Perc)} ||
        Perc <- P].
