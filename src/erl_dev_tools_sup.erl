%%%-------------------------------------------------------------------
%% @doc dev_tools top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(erl_dev_tools_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 5, 60},
          [{user_action,
            {user_action, start_link, []},
            permanent, 1000, worker, [user_action]},
           {fswatch_buf,
            {fswatch_buf, start_link, []},
            permanent, 1000, worker, [fswatch_buf]},
           {fswatch_sup,
            {fswatch_sup, start_link, []},
            permanent, 1000, supervisor, [fswatch]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
