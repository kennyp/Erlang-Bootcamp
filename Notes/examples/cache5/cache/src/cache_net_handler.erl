-module(cache_net_handler).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {sock}).

start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).

init([Sock]) ->
    {ok, #state{sock=Sock}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(go, #state{sock=Sock}=State) ->
    inet:setopts(Sock, [{active, once}]),
    {noreply, State};
handle_info({tcp, Sock, Data}, State) ->
    case process_command(string:tokens(Data, " \r\n"), Sock) of
        stop ->
            {stop, normal, State};
        _ ->
            inet:setopts(Sock, [{active, once}]),
            {noreply, State, 10000}
    end;
handle_info({tcp_closed, _Sock}, State) ->
    error_logger:info_msg("Client closed connection~n"),
    {stop, normal, State};
handle_info({tcp_error, _Sock, Reason}, State) ->
    error_logger:info_msg("Network error: ~p~n", [Reason]),
    {stop, normal, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
process_command(["put", Key, Value], Sock) ->
    cache:put(Key, Value),
    gen_tcp:send(Sock, "+ok\r\n"),
    ok;
process_command(["get", Key], Sock) ->
    case cache:get(Key) of
        not_found ->
            gen_tcp:send(Sock, "-err\r\n");
        {ok, Value} ->
            io:format("Value: ~p~n", [Value]),
            gen_tcp:send(Sock, ["+ok ", Value, "\r\n"])
    end,
    ok;
process_command(["del", Key], Sock) ->
    cache:del(Key),
    gen_tcp:send(Sock, "+ok\r\n"),
    ok;
process_command(_, _Sock) ->
    stop.
