-module(cache_fe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    CacheFE = {cache_fe, {cache_fe, start_link, []},
               permanent, 2000, worker, [cache_fe]},
    CacheNet = {cache_net, {cache_net_listener, start_link, []},
                permanent, 2000, worker, [cache_net_listener]},
    CacheHandlerSup = {cache_net_handler_sup, {cache_net_handler_sup, start_link, []},
                       permanent, infinity, supervisor, [cache_net_handler_sup]},


    {ok, {{rest_for_one, 10, 60}, [CacheFE,
                                   CacheHandlerSup,
                                   CacheNet]}}.

%% Internal functions
