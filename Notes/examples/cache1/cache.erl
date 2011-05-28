-module(cache).

-behaviour(gen_server).

-define(DEFAULT_TTL, 15000).

%% API
-export([start_link/0,
         put/4,
         put/3,
         del/2,
         get/2,
         size/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {cache=dict:new()}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

put(Cache, Key, Value) ->
    cache:put(Cache, Key, Value, ?DEFAULT_TTL).

put(Cache, Key, Value, TTL) ->
    gen_server:cast(Cache, {put, Key, Value, TTL}).

del(Cache, Key) ->
    gen_server:cast(Cache, {del, Key}).

get(Cache, Key) ->
    gen_server:call(Cache, {get, Key}).

size(Cache) ->
    gen_server:call(Cache, size).

init([]) ->
    {ok, #state{}}.

handle_call({get, Key}, From, #state{cache=Cache}=State) ->
    case dict:find(Key, Cache) of
        error ->
            {reply, not_found, State};
        {ok, {Entry, _}} ->
            Entry ! {fetch, From},
            {noreply, State}
    end;
handle_call(size, _From, #state{cache=Cache}=State) ->
    {reply, dict:size(Cache) div 2, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({put, Key, Value, TTL}, #state{cache=Cache}=State) ->
    State1 = case dict:is_key(Key, Cache) of
                 true ->
                     State;
                 false ->
                     Entry = spawn(fun() -> entry(Key, Value, TTL) end),
                     MRef = erlang:monitor(process, Entry),
                     Cache1 = dict:store(Entry, Key, Cache),
                     State#state{cache=dict:store(Key, {Entry, MRef}, Cache1)}
             end,
    {noreply, State1};
handle_cast({del, Key}, #state{cache=Cache}=State) ->
    State1 = case dict:is_key(Key, Cache) of
                false ->
                    State;
                true ->
                     {Entry, MRef} = dict:fetch(Key, Cache),
                     erlang:demonitor(MRef, [flush]),
                     Entry ! eject,
                     Cache1 = dict:erase(Entry, Cache),
                     State#state{cache=dict:erase(Key, Cache1)}
            end,
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Entry, _Info}, #state{cache=Cache}=State) ->
    State1 = case dict:find(Entry, Cache) of
                 {ok, Key} ->
                     error_logger:info_msg("Removing ~p due to TTL expiry~n", [Key]),
                     Cache1 = dict:erase(Key, Cache),
                     State#state{cache=dict:erase(Entry, Cache1)};
                 error ->
                     State
             end,
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
entry(Key, Value, TTL) ->
    timer:send_after(TTL, eject),
    entry(Key, Value).

entry(Key, Value) ->
    receive
        {fetch, Caller} ->
            gen_server:reply(Caller, {ok, Value}),
            entry(Key, Value);
        eject ->
            error_logger:info_msg("Key ~p ejected~n", [Key]),
            ok
    end.
