%% fswatch will not get killed if we accidentally do a Ctrl-C in the
%% erlang shell
-module(fswatch).

-behaviour(gen_server).

%% API
-export([start_link/1,
         stop/1]).

%% Exported for testing
-export([port_os_pid/1,
         start_port/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([find_executable/0]).

-define(ENV_WATCH_DIRS, "WATCH_DIRS").
-define(DETS_FILE, "/tmp/erl-dev-tools/dets_fswatch").

-record(state, {dir :: string(),
                port :: port()}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
start_link(Dir) ->
    gen_server:start_link(?MODULE, Dir, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

port_os_pid(Pid) ->
    gen_server:call(Pid, port_os_pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Dir) ->
    %% setup trap_exit true otherwise terminate is not called on
    %% application stop or supervisor:termniate_child
    process_flag(trap_exit, true),
    Port = start_port(Dir),
    {ok, #state{dir = Dir, port = Port}}.

handle_call(stop, _From, State) ->
    {stop, normal, stop, State};

handle_call(port_os_pid, _From, #state{port = Port} = State) ->
    Reply = os_pid(Port),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, _S}}, #state{port = Port} = State) ->
    {stop, port_died, State};
handle_info({_Port, {data, {eol, RawDataString}}}, State) ->
    [Path|Flags] = parse_raw_input(RawDataString),
    register_change(Path, Flags),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(port_died, _State)  ->
    ok;

terminate(_Reason, #state{port = Port}) when is_port(Port) ->
    kill_os_process(os_pid(Port)),
    erlang:port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
register_change(Path, Flags) ->
    case is_ignored_file(Path) of
        true ->
            ignore;
        false ->
            fswatch_buf:register_change(Path, Flags),
            ok
    end.


is_ignored_file(Path) ->
    case {is_flymake_file(Path), is_erl_file(Path)} of
        {false, true} -> false;
        {_, _} -> true
    end.


is_erl_file(Path) ->
    filename:extension(Path) == ".erl".

is_flymake_file(Path) ->
    is_string_contains(Path, "_flymake.").

is_string_contains(String, Substring) ->
    string:rstr(String, Substring) > 0.

parse_raw_input(RawDataString) ->
    string:tokens(RawDataString, " ").

find_executable() ->
    os:find_executable("fswatch").

start_port(Path) ->
    Port =
        erlang:open_port({spawn_executable, find_executable()},
                         [stream,
                          exit_status,
                          {line, 16384},
                          {args, ["--event-flags", "--monitor=fsevents_monitor", Path]},
                          {cd, Path}]),
    log_process_created(Port),
    Port.

os_pid(Port) ->
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    OsPid.

kill_os_process(OsPid) ->
    Cmd = "kill -9 " ++ integer_to_list(OsPid),
    os:cmd(Cmd),
    ok.

log_process_created(Port) ->
    ok = filelib:ensure_dir(?DETS_FILE),
    Table = dets_fswatch,
    {ok, Table} = dets:open_file(Table, [{file, ?DETS_FILE}]),
    ok = dets:insert(Table, {os_pid(Port), os:timestamp()}),
    ok = dets:close(Table),
    ok.

-ifdef(TEST).
register_change_test() ->
    ok = register_change(".../abc.erl", []),
    ignore = register_change(".../abc.beam", []),
    ignore = register_change(".../abc", []),
    ignore = register_change(".../abc/", []),
    ignore = register_change(".../abc_flymake.erl", []).


-endif.
