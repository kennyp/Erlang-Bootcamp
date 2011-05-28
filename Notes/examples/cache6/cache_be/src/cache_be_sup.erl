-module(cache_be_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(FrontEndNode) ->
    wait_for_node(FrontEndNode),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [FrontEndNode]).

init([FrontEndNode]) ->
    Cache = {cache, {cache, start_link, [FrontEndNode]},
             permanent, 2000, worker, [cache]},
    {ok, {{one_for_one, 10, 60}, [Cache]}}.

%% Internal functions
wait_for_node(FrontEndNode) ->
    case net_adm:ping(FrontEndNode) of
        pong ->
            ok;
        pang ->
            wait_for_node(FrontEndNode)
    end.
