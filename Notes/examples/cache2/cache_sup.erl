-module(cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Cache = {cache, {cache, start_link, []},
             permanent, 2000, worker, [cache]},

    {ok, {{one_for_one, 10, 60}, [Cache]}}.

%% Internal functions
