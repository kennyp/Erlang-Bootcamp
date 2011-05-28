-module(cache).

-behaviour(gen_server).

-record(state, {cache=dict:new()}).

-define(SERVER, ?MODULE).
-define(TTL, 5000).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% client functions
-export([get/1, put/2, del/1, count/0]).

get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

put(Key, Value) ->
    gen_server:cast(?SERVER, {put, Key, Value}).

del(Key) ->
    gen_server:cast(?SERVER, {del, Key}).

count() ->
    gen_server:call(?SERVER, size).

%% gen_server functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    io:format("Starting the cache~n"),
    {ok, #state{}}.

handle_call(size, _From, #state{cache=Cache}=State) ->
    {reply, dict:size(Cache), State};

handle_call({get, Key}, _From, #state{cache=Cache}=State) ->
    case dict:find(Key, Cache) of
        {ok, {Value, Stamp}}  ->
            case timer:now_diff(erlang:now(), Stamp) div 1000 < ?TTL of
                true -> {reply, Value, State};
                false -> {reply, not_found, State#state{cache=dict:erase(Key, Cache)}}
            end;
        error ->
            {reply, not_found, State}
    end.

handle_cast({put, Key, Value}, #state{cache=Cache}=State) ->
    {noreply, State#state{cache=dict:store(Key, {Value, erlang:now()}, Cache)}};

handle_cast({del, Key}, #state{cache=Cache}=State) ->
    {noreply, State#state{cache=dict:erase(Key, Cache)}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
