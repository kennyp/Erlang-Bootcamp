
-module(cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Cache = {cache, {cache, start_link, []},
        permanent, 2000, worker, [cache]},
    CacheSock = {cache_socket_listener, {cache_socket_listener, start_link, []},
        permanent, 2000, worker, [cache_socket_listener]},
    {ok, {{one_for_one, 10, 60}, [Cache, CacheSock]}}.

