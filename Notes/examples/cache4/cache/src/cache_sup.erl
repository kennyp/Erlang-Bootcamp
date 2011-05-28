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
    CacheNet = {cache_net, {cache_net_listener, start_link, []},
                permanent, 2000, worker, [cache_net_listener]},

    {ok, {{rest_for_one, 10, 60}, [Cache,
                                   CacheNet]}}.

%% Internal functions
