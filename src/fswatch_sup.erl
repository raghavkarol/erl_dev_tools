-module(fswatch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 5, 60},
          [{fswatch,
            {fswatch, start_link, []},
            permanent, 1000, worker, [fswatch]}]}}.
