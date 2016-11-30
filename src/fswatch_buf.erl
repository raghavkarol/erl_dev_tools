-module(fswatch_buf).

-behaviour(gen_server).

%% API
-export([start_link/0,
         flush/0,
         register_change/2]).

%% API for testing only
-export([wait_get_changes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type path() :: string().
-type props() :: [proplists:property()].

-record(state, {waiting_proc :: undefined | {pid(), Tag:: term()},
                changes = [] :: [{path(), props()}]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_change(Path, Flags) ->
    gen_server:cast(?MODULE, {register_change, Path, Flags}).

flush() ->
    gen_server:call(?MODULE, flush).

%% @doc
%% testing only
wait_get_changes() ->
    gen_server:call(?MODULE, wait_get_changes).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(wait_get_changes, From, #state{waiting_proc = undefined, changes = []} = State) ->
    {noreply, State#state{waiting_proc = From}};

handle_call(wait_get_changes, From, #state{changes = _} = State) ->
    erlang:send_after(0, ?MODULE, reply_wait_get_changes),
    {noreply, State#state{waiting_proc = From}};

handle_call(flush, _From, #state{changes = Changes} = State) ->
    {reply, changed_paths(Changes), State#state{changes = []}}.


handle_cast({register_change, Path, Flags}, State) ->
    #state{changes = Changes} = State,
    Props = [{flags, Flags}],
    Changes1 = lists:keystore(Path, 1, Changes, {Path, Props}),
    case State#state.waiting_proc of
        undefined ->
            ok;
        _ ->
            erlang:send_after(0, ?MODULE, reply_wait_get_changes)
    end,
    State1 = State#state{changes = Changes1},
    {noreply, State1}.

handle_info(reply_wait_get_changes, #state{waiting_proc = From,
                                           changes = Changes} = State)

 when From =/= undefined ->
    gen_server:reply(State#state.waiting_proc, changed_paths(Changes)),
    {noreply, State#state{waiting_proc = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
changed_paths(Changes) ->
    lists:usort([Path0 || {Path0, _Props} <- Changes]).
