-module(thing_sup_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_thing/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_thing() ->
    supervisor:start_child(?SERVER, []).

init([]) ->
    Child = [{ thing, { thing, start_link, []},
              temporary, 2000, worker, [thing]}],
    {ok, { {simple_one_for_one, 0, 1}, Child} }.
