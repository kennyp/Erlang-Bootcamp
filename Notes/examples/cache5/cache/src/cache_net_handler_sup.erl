-module(cache_net_handler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_handler/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_handler(Sock) ->
    case supervisor:start_child(?SERVER, [Sock]) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Sock, Pid),
            Pid ! go,
            {ok, Pid};
        Error ->
            Error
    end.

init([]) ->
    Child = [{ cache_net_handler, { cache_net_handler, start_link, []},
              temporary, 2000, worker, [cache_net_handler]}],
    {ok, { {simple_one_for_one, 0, 1}, Child} }.
