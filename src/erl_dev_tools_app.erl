%%%-------------------------------------------------------------------
%% @doc dev_tools public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_dev_tools_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(ENV_WATCH_DIRS, "WATCH_DIRS").
%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    {ok, Pid} = erl_dev_tools_sup:start_link(),
    start_fswatch(),
    case os:getenv("ERL_DEV_TOOLS_WATCH_CHANGES") of
        V when V == "1";
               V == "true" ->
            user_action:watch_changes();
        _ ->
            ok
    end,
    {ok, Pid}.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_fswatch() ->
    {ok, Cwd} = file:get_cwd(),
    WatchDirsStr = util:getenv(?ENV_WATCH_DIRS, Cwd),
    Dirs = util:get_watch_dirs(WatchDirsStr),
    [{ok, _Pid} = supervisor:start_child(fswatch_sup, [Dir]) || Dir <- Dirs],
    ok.
