%%% The poor man's profiler, profiles on wall clock time.
-module(pprof).

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
         analyse_call/2,
         analyse_slowest/0,
         analyse_slowest/1,
         describe_call/1,
         pretty_print/1,
         reset/0,
         slowest/0,
         slowest/1,
         stop_tracer/0,
         top_calls/1,
         top_calls/2
        ]).

%% tracer callback
-export([trace/2]).

%% gen_server callbacks
-export([init/1, callback_mode/0, handle_event/4,
         terminate/3, code_change/3]).

-include("pprof.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, pprof_result).

-record(state, {max_calls = 100, num_calls = 0}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec add(Mod::atom()) -> ok.
add(Mod) ->
    dbg:tpl(Mod, [{'_', [], [{return_trace}]}]).

-spec add(Mod::atom(), Fun::atom(), Arity::pos_integer()) -> ok.
add(Mod, Fun, Arity) ->
    dbg:tpl(Mod, Fun, Arity, [{'_', [], [{return_trace}]}]).

top_calls(accumulated_time) ->
    top_calls(accumulated_time, 10);

top_calls(count) ->
    top_calls(accumulated_time, 10).

top_calls(Type, N)
  when Type == accumulated_time;
       Type == count ->
    gen_statem:call(?SERVER, {top_calls, Type, N}).

slowest() ->
    slowest(10).

slowest(N) ->
    gen_statem:call(?SERVER, {slowest, N}).

analyse_slowest() ->
    gen_statem:call(?SERVER, {analyse_slowest, bfs}).

analyse_slowest(Type)
  when Type == bfs;
       Type == dfs ->
    gen_statem:call(?SERVER, {analyse_slowest, Type}).

analyse_call(Id, Type)
  when Type == bfs;
       Type == dfs ->
    gen_statem:call(?SERVER, {analyse_call, Id, Type}).

stop_tracer() ->
    dbg:ctp(),
    dbg:stop_clear().

describe_call(Id) ->
    gen_statem:call(?SERVER, {describe_call, Id}).

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

reset() ->
    gen_statem:call(?SERVER, reset).


%% -----------------------------------------------------------------------------
%% Pretty printing
%% -----------------------------------------------------------------------------
pretty_print({bfs, G}) ->
    Sep = lists:duplicate(60, "-") ++ "\n",
    [
     Sep,
     pretty_print_title(),
     Sep,
     lists:map(fun({_, []}) ->
                       [];
                  ({Call, Called}) ->
                       [ pretty_print_call(Call), [pretty_print_call(1, C) || C <- Called],
                         Sep
                       ]
                  end,
               G)
    ];

pretty_print({dfs, _} = CG) ->
    Sep = lists:duplicate(60, "-") ++ "\n",
    [Sep, pretty_print_title(), Sep, pretty_print(0, CG), Sep];

pretty_print(#call{} = Call) ->
    #call{mfa = {M, F, Args}, result = Result, time = Time} = Call,
    [
     to_string(M),
     ":",
     to_string(F),
     "(", string:join([io_lib:format("~p", [A]) || A <- Args], ", "), ")",
     "->", io_lib:format("~p", [Result]),
     " in ",
     to_string(Time), " msecs"
    ].


pretty_print(Indent, {dfs, {Call, Called}}) ->
    [ pretty_print_call(Indent, Call),
     [ [pretty_print(Indent+1, {dfs, {Call1, Called1}})] || {Call1, Called1} <- Called] ].

pretty_print_call(#call{} = Call) ->
    pretty_print_call(0, #call{} = Call).

pretty_print_title() ->
    call_line(0, "id", "time", "call").

pretty_print_call(Indent, #call{} = Call) ->
    #call{id = Id, mfa = MFA, time = Time} = Call,
    call_line(Indent, Id, Time, pretty_print_mfa(MFA)).

call_line(Indent, Id, Time, MFA) ->
    Sep = " ",
    End = "\n",
    [width(to_string(Id), 10),
     Sep,
     width(to_string(Time), 8),
     Sep,
     lists:duplicate(Indent, " "),
     Sep,
     width(MFA, -30),
     End].

pretty_print_mfa({M, F, A}) ->
    io_lib:format("~p:~p/~p", [M, F, length(A)]).

width(T, W) ->
    lists:flatten(io_lib:format("~" ++ to_string(W) ++ "s", [to_string(T)])).

to_string(A) when is_atom(A) ->
    atom_to_list(A);

to_string(N) when is_integer(N) ->
    integer_to_list(N);

to_string(L) ->
    L.

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

    new_process_table(),
    dbg:tracer(process, {fun pprof:trace/2, trace_state}),
    dbg:p(all, call),

    {ok, ready, new_state(MaxCalls)}.

handle_event({call, From}, {trace, _, call, _}, limit, _Data) ->
    Actions = [tracer_reply(From)],
    {keep_state_and_data, Actions};

handle_event({call, From}, {trace, _, return_from, _, _}, limit, _Data) ->
    Actions = [tracer_reply(From)],
    {keep_state_and_data, Actions};

handle_event({call, From}, {trace, _, call, _} = Trace, ready, Data) ->
    {trace, Pid, call, MFArgs} = Trace,
    Now = current_time(),
    Process = lookup_process(Pid),
    Process1 = update_process(call, Now, Pid, MFArgs, Process),
    Actions = [tracer_reply(From)],
    save_process(Process1),
    {keep_state, Data, Actions};

handle_event({call, From}, {trace, _, return_from, _, _} = Trace, ready, Data) ->
    {trace, Pid, return_from, MFArity, Result} = Trace,
    Now = current_time(),
    Process = lookup_process(Pid),
    Process1 = update_process(return, Now, Pid, {MFArity, Result}, Process),
    Actions = [tracer_reply(From)],
    save_process(Process1),
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
    Data1 = new_state(Data#state.max_calls),
    delete_all_processes(),
    {keep_state, Data1, [{reply, From, ok}]};

handle_event({call, From}, {top_calls, accumulated_time, N}, _State, Data) ->
    Result = foldl_processes(fun accumulated_time/2),
    Result1 = top(N, 2, Result),
    {keep_state, Data, [{reply, From, Result1}]};

handle_event({call, From}, {top_calls, count, N}, _State, Data) ->
    Result = foldl_processes(fun count/2),
    Result1 = top(N, 2, Result),
    {keep_state, Data, [{reply, From, Result1}]};

handle_event({call, From}, {slowest, N}, _State, Data) ->
    Result = foldl_processes(fun slowest/2),
    Result1 = lists:flatten([maps:values(R) || R <- Result]),
    Result2 = top(N, #call.time, Result1),
    {keep_state, Data, [{reply, From, Result2}]};

handle_event({call, From}, {analyse_slowest, Type}, _State, Data) ->
    Result = foldl_processes(fun slowest/2),
    Result1 = lists:flatten([maps:values(R) || R <- Result]),
    Result2 = top(1, #call.time, Result1),
    Reply =
        case Result2 of
            [] ->
                none;
            [Slowest|_] ->
                call_graph(Type, Slowest, Result1)
        end,
    {keep_state, Data, [{reply, From, Reply}]};

handle_event({call, From}, {describe_call, Id}, _State, Data) ->
    %%
    %% TODO: Consider making a single global map for all calls instead
    %% of per process
    %%
    Result = ets:foldl(
               fun(#process{calls = Calls}, undefined) ->
                       case maps:find(Id, Calls) of
                           error ->
                               undefined;
                           {ok, Value} ->
                               Value
                       end;
                  (_, Value) ->
                       Value
               end,
               undefined,
               ?TABLE),
    {keep_state, Data, [{reply, From, Result}]};
handle_event({call, From}, {analyse_call, Id, Type}, _State, Data) ->
    Result = foldl_processes(fun slowest/2),
    Result1 = lists:flatten([maps:values(R) || R <- Result]),
    Reply =
    case lists:keyfind(Id, #call.id, Result1) of
        false ->
            not_found;
        Call ->
            call_graph(Type, Call, Result1)
    end,
    {keep_state, Data, [{reply, From, Reply}]};
handle_event({call, From}, Event, State, _Data) ->
    io:format("unexpected event: ~p in State: ~p", [Event, State]),
    {keep_state_and_data, [{reply, From, ok}]}.


terminate(_Reason, _State, _Data) ->
    stop_tracer(),
    delete_process_table(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

%% BFS call graph
call_graph(bfs, Call, Calls) ->
    {bfs, call_graph_bfs(Call, Calls)};
call_graph(dfs, Call, Calls) ->
    {dfs, call_graph_dfs(Call, Calls)}.

call_graph_bfs(Call, Calls) ->
    Called = find_called(Call, Calls),

    [{Call, Called}] ++ lists:flatten([call_graph_bfs(C, Calls) || C <- Called]).

call_graph_dfs(Call, Calls) ->
    Called = find_called(Call, Calls),
    case Called of
        [] ->
            {Call, []};
        _ ->
            {Call, [call_graph_dfs(C, Calls) || C <- Called]}
    end.

find_called(#call{id = Id1}, Calls) ->
    Called = lists:filter(fun(#call{caller = Id}) ->
                         Id1 == Id
                 end,
                          Calls),
    lists:keysort(#call.t1, Called).


top(N, Keypos, TupleList) ->
    L1 = lists:keysort(Keypos, TupleList),
    L2 = lists:reverse(L1),
    lists:sublist(L2, N).

tracer_reply(From) ->
    {reply, From, trace_state}.

%% Process state update functions

update_process(call, Now, Pid, MFArgs, undefined) ->
    Call = #call{caller = none, t1 = Now, mfa = MFArgs},
    Id = key(Now, Pid, MFArgs),
    Call1 = Call#call{id = Id},
    #process{pid = Pid, stack = [Call1]};

update_process(call, Now, Pid, MFArgs, Proc = #process{stack = []}) ->
    Call = #call{caller = none, t1 = Now, mfa = MFArgs},
    Id = key(Now, Pid, MFArgs),
    Call1 = Call#call{id = Id},
    Proc#process{stack = [Call1]};

update_process(call, Now, Pid, MFArgs, Proc = #process{stack = Stack})
  when Stack =/= [] ->
    Call = #call{caller = none, t1 = Now, mfa = MFArgs},
    Id = key(Now, Pid, MFArgs),
    #call{id = CallerId} = hd(Stack),
    Call1 = Call#call{id = Id, caller = CallerId},
    Proc#process{stack = [Call1|Stack]};

update_process(return, Now, _Pid, {MFArity, Result}, Proc = #process{stack = Stack}) ->
    Top = hd(Stack),
    {M, F, _} = MFArity,
    Elapsed = Now - Top#call.t1,
    Call = Top#call{t2 = Now, time = Elapsed, result = Result},

    Calls = Proc#process.calls,
    Calls1 = Calls#{Call#call.id => Call},

    Time =  Proc#process.time,
    Time1 = maps:update_with({M, F}, fun(T) -> T + Elapsed end, Elapsed, Time),

    Count = Proc#process.count,
    Count1 = maps:update_with({M, F}, fun(N) -> N + 1 end, 1, Count),

    Proc#process{stack = tl(Stack), calls = Calls1, time = Time1, count = Count1}.

%% tracer state update
update_state(#state{num_calls = Num} = State) ->
    State#state{num_calls = Num + 1}.

new_state(MaxCalls) ->
    #state{max_calls = MaxCalls}.

is_tracer_limit(#state{num_calls = Num, max_calls = Max}) ->
    Num >= Max.

%% State management using ETS
key(Now, Pid, MFA) ->
    erlang:phash2({Now, Pid, MFA}).

new_process_table() ->
    ets:new(?TABLE, [named_table, {keypos, #process.pid}]).

lookup_process(Pid) ->
    case ets:lookup(?TABLE, Pid) of
        [] ->
            undefined;
        [Process] ->
            Process
    end.

save_process(Process) ->
    ets:insert(?TABLE, Process).

delete_process_table() ->
    ets:delete(?TABLE).

delete_all_processes() ->
    ets:delete_all_objects(?TABLE).

foldl_processes(Fun) ->
    Result = ets:foldl(Fun, [], ?TABLE),
    lists:flatten(Result).

%% ets fold functions
accumulated_time(#process{time = Time}, Acc) ->
    [maps:to_list(Time)|Acc].

count(#process{count = Count}, Acc) ->
    [maps:to_list(Count)|Acc].

slowest(#process{calls = Calls}, Acc) ->
    [Calls| Acc].

%% Utility
current_time() ->
    erlang:system_time(millisecond).

maybe_stop_tracer(#state{num_calls = N, max_calls = Max}) when N >= Max ->
    stop_tracer();

maybe_stop_tracer(_) ->
    ok.
