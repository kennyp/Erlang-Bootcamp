-module(cache_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(cache).

start(_StartType, _StartArgs) ->
    cache_sup:start_link().

stop(_State) ->
    ok.
