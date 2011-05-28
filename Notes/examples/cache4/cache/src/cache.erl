-module(cache).

-behaviour(gen_server).

-define(DEFAULT_TTL, 15000).

%% API
-export([start_link/0,
         put/3,
         put/2,
         del/1,
         get/1,
         size/0,
         crash/0,
         bulk_up/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {cache=dict:new()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
    cache:put(Key, Value, ?DEFAULT_TTL).

put(Key, Value, TTL) ->
    gen_server:cast(?MODULE, {put, Key, Value, TTL}).

del(Key) ->
    gen_server:cast(?MODULE, {del, Key}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

size() ->
    gen_server:call(?MODULE, size).

crash() ->
    gen_server:cast(?MODULE, crash).

bulk_up() ->
    Before = length(erlang:processes()),
    [cache:put(random_string(5), random_string(3)) || _ <- lists:seq(1, 100)],
    After = length(erlang:processes()),
    {Before, After}.

init([]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("Cache starting: ~p~n", [self()]),
    {ok, #state{}}.

handle_call({get, Key}, From, #state{cache=Cache}=State) ->
    case dict:find(Key, Cache) of
        error ->
            {reply, not_found, State};
        {ok, Entry} ->
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
                     Entry = spawn_link(fun() -> entry(Key, Value, TTL) end),
                     Cache1 = dict:store(Entry, Key, Cache),
                     State#state{cache=dict:store(Key, Entry, Cache1)}
             end,
    {noreply, State1};
handle_cast({del, Key}, #state{cache=Cache}=State) ->
    State1 = case dict:is_key(Key, Cache) of
                false ->
                    State;
                true ->
                     Entry = dict:fetch(Key, Cache),
                     Entry ! eject,
                     Cache1 = dict:erase(Entry, Cache),
                     State#state{cache=dict:erase(Key, Cache1)}
            end,
    {noreply, State1};
handle_cast(crash, State) ->
    exit(self(), kill),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Entry, _Reason}, #state{cache=Cache}=State) ->
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

random_string(Length) ->
    [$` + random:uniform(26) || _ <- lists:seq(1, Length)].
