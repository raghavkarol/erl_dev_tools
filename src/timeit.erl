%% https://gist.githubusercontent.com/gburd/2656289/raw/c25e232e7f966a86ea251524eb93ce033ca1b906/timeit.erl
-module(timeit).
-compile(export_all).

%% @doc Dynamically add timing to MFA.  There are various types of
%% timing.
%%
%% all - time latency of all calls to MFA
%%
%% {sample, N, Max} - sample every N calls and stop sampling after Max
%%
%% {threshold, Millis, Max} - count # of calls where latency is > Millis
%% and count # of calls total, thus percentage of calls over threshold
-record(state, {dict = orddict:new(),
               indent = 0}).

timeit(Mod, Fun, Arity, Type) ->
    Type2 = case Type of
                {sample, N, Max} -> {sample, {N, Max}, {0, 0, 0}};
                {threshold, Millis, Max} -> {threshold, {Millis, Max}, {0, 0}};
                {all, Max} -> {all, {0, Max}}
            end,
    dbg:tracer(process, {fun trace/2, {#state{}, Type2}}),
    dbg:p(all, call),
    dbg:tpl(Mod, Fun, Arity, [{'_', [], [{return_trace}]}]).

stop() -> dbg:stop_clear().

trace({trace, Pid, call, {Mod, Fun, _Args}}, {State, {all, {Count, Max}}}) ->
    #state{dict=D, indent=Indent} = State,
    D2 = orddict:store({Pid, Mod, Fun}, now(), D),
    State1 = State#state{dict=D2, indent=Indent+1},
    {State1, {all, {Count, Max}}};
trace({trace, Pid, call, {Mod, Fun, _}},
      {State, {sample, {N, Max}, {M, K, Total}}}) ->
    #state{dict=D, indent=Indent} = State,
    M2 = M+1,
    Total2 = Total+1,
    if N == M2 ->
            D2 = orddict:store({Pid, Mod, Fun}, now(), D),
            State1 = State#state{dict=D2, indent=Indent+1},
            {State1, {sample, {N, Max}, {0, K, Total2}}};
       true ->
            {State, {sample, {N, Max}, {M2, K, Total2}}}
    end;
trace({trace, Pid, call, {Mod, Fun, _Args}},
      {State, {threshold, {Millis, Max}, {Over, Total}}}) ->
    #state{dict=D, indent=Indent} = State,
    D2 = orddict:store({Pid, Mod, Fun}, now(), D),
    State1 = State#state{dict=D2, indent=Indent+1},
    {State1, {threshold, {Millis, Max}, {Over, Total+1}}};

trace({trace, Pid, return_from, {Mod, Fun, Args}, _Result},
      Acc={State, {all, {Count, Max}}}) ->
    #state{dict=D, indent=Indent} = State,
    Key = {Pid, Mod, Fun},
    case orddict:find(Key, D) of
        {ok, StartTime} ->
            Count2 = Count+1,
            ElapsedUs = timer:now_diff(now(), StartTime),
            ElapsedMs = ElapsedUs/1000,
            _IndentSpacs = lists:flatten([" " || _ <- lists:seq(1, Indent)]),
            io:format(user, "~p:~p:~p: ~p ~p ms\n", [Pid, Mod, Fun, Args, ElapsedMs]),
            if Count2 == Max -> stop();
               true ->
                    D2 = orddict:erase(Key, D),
                    State1 = State#state{dict=D2, indent=Indent-1},
                    {State1, {all, {Count2, Max}}}
            end;
        error -> Acc
    end;
trace({trace, Pid, return_from, {Mod, Fun, _}, _Result},
      Acc={State, {sample, {N, Max}, {M, K, Total}}}) ->
    #state{dict=D, indent=Indent} = State,
    Key = {Pid, Mod, Fun},
    case orddict:find(Key, D) of
        {ok, StartTime} ->
            K2 = K+1,
            ElapsedUs = timer:now_diff(now(), StartTime),
            ElapsedMs = ElapsedUs/1000,
            io:format(user, "[sample ~p/~p] ~p:~p:~p: ~p ms\n",
                      [K2, Total, Pid, Mod, Fun, ElapsedMs]),
            if K2 == Max -> stop();
               true ->
                    D2 = orddict:erase(Key, D),
                    State1 = State#state{dict=D2, indent=Indent-1},
                    {State1, {sample, {N, Max}, {M, K2, Total}}}
            end;
        error -> Acc
    end;
trace({trace, Pid, return_from, {Mod, Fun, _}, _Result},
      Acc={State, {threshold, {Millis, Max}, {Over, Total}}}) ->
    Key = {Pid, Mod, Fun},
    #state{dict=D, indent=Indent} = State,
    case orddict:find(Key, D) of
        {ok, StartTime} ->
            ElapsedUs = timer:now_diff(now(), StartTime),
            ElapsedMs = ElapsedUs / 1000,
            Over2 =
                if ElapsedMs > Millis ->
                        io:format(user, "[over threshold ~p, ~p/~p] ~p:~p:~p: ~p ms\n",
                                  [Millis, Over+1, Total, Pid, Mod, Fun, ElapsedMs]),
                        Over+1;
                   true ->
                        Over
                end,
            if Max == Over ->
                    stop();
               true ->
                    D2 = orddict:erase(Key, D),
                    State1 = State#state{dict=D2, indent=Indent-1},
                    {State1, {threshold, {Millis, Max}, {Over2, Total}}}
            end;
        error -> Acc
    end.
