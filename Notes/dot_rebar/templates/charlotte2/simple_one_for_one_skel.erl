-module({{name}}_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         {{fn}}/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

{{fn}}() ->
    supervisor:start_child(?SERVER, []).

init([]) ->
    Child = [{ {{name}}, { {{name}}, start_link, []},
              temporary, 2000, worker, [{{name}}]}],
    {ok, { {simple_one_for_one, 0, 1}, Child} }.
