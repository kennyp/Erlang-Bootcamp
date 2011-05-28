-module(cache_fe_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(cache_fe).

start(_StartType, _StartArgs) ->
    cache_fe_sup:start_link().

stop(_State) ->
    ok.
