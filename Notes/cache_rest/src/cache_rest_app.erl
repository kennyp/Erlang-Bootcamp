%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the cache_rest application.

-module(cache_rest_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for cache_rest.
start(_Type, _StartArgs) ->
    cache_rest_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for cache_rest.
stop(_State) ->
    ok.
