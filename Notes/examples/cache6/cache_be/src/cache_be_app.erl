-module(cache_be_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(cache_be).

start(_StartType, _StartArgs) ->
    cache_be_sup:start_link('cache_fe@127.0.0.1').

stop(_State) ->
    ok.
