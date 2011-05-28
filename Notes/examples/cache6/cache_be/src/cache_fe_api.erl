-module(cache_fe_api).

-export([register_cache/2]).

register_cache(CachePid, FrontEndNode) ->
    Pid = wait_for_front_end(FrontEndNode),
    {Pid, gen_server:call({cache_fe, FrontEndNode}, {register_cache, CachePid})}.

wait_for_front_end(FrontEndNode) ->
    case rpc:call(FrontEndNode, erlang, whereis, [cache_fe]) of
        undefined ->
            timer:sleep(100),
            wait_for_front_end(FrontEndNode);
        P when is_pid(P) ->
            erlang:link(P),
            P
    end.
