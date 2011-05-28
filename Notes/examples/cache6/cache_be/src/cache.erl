-module(cache).

-behaviour(gen_server).

-define(DEFAULT_TTL, 15000).

%% API
-export([start_link/1,
         put/3,
         put/2,
         del/1,
         get/1,
         get/2,
         keys/1,
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

-record(state, {fe,
                cache=dict:new()}).

start_link(FrontEndNode) ->
    gen_server:start_link({local, ?MODULE} ,?MODULE, [FrontEndNode], []).

put(Key, Value) ->
    cache:put(Key, Value, ?DEFAULT_TTL).

put(Key, Value, TTL) ->
    gen_server:cast(?MODULE, {put, Key, Value, TTL}).

del(Key) ->
    gen_server:cast(?MODULE, {del, Key}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

get(Seed, Key) ->
    gen_server:call(Seed, {get_ttl, Key}).

keys(Seed) ->
    gen_server:call(Seed, keys).

size() ->
    gen_server:call(?MODULE, size).

crash() ->
    gen_server:cast(?MODULE, crash).

bulk_up() ->
    Before = length(erlang:processes()),
    [cache:put(random_string(5), random_string(3)) || _ <- lists:seq(1, 100)],
    After = length(erlang:processes()),
    {Before, After}.

init([FrontEndNode]) ->
    process_flag(trap_exit, true),
    case cache_fe_api:register_cache(self(), FrontEndNode) of
        {FE, ok} ->
            error_logger:info_msg("Cache starting: ~p~n", [self()]),
            {ok, #state{fe=FE}};
        {FE, {sync, Seed}} ->
                error_logger:info_msg("Cache seeding: ~p, ~p~n", [self(), Seed]),
            Me = self(),
            spawn(fun() -> sync_caches(Seed, Me) end),
            {ok, #state{fe=FE}}
    end.

handle_call(keys, _From, #state{cache=Cache}=State) ->
    Keys = [Key || Key <- dict:fetch_keys(Cache),
                   is_pid(Key) =:= false],
    {reply, Keys, State};
handle_call({get_ttl, Key}, From, #state{cache=Cache}=State) ->
    case dict:find(Key, Cache) of
        error ->
            {reply, not_found, State};
        {ok, Entry} ->
            Entry ! {fetch_ttl, From},
            {noreply, State}
    end;
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
handle_info({'EXIT', FE, _Reason}, #state{fe=FE}=State) ->
    error_logger:error_msg("Front end crashed. Restarting cache...~n"),
    exit(self(), kill),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
entry(Key, Value, TTL) ->
    timer:send_after(TTL, eject),
    entry(erlang:now(), TTL, Key, Value).

entry(Start, TTL, Key, Value) ->
    receive
        {fetch, Caller} ->
            gen_server:reply(Caller, {ok, Value}),
            entry(Start, TTL, Key, Value);
        {fetch_ttl, Caller} ->
            EffectiveTTL = TTL - (timer:now_diff(erlang:now(), Start) div 1000),
            gen_server:reply(Caller, {ok, Value, EffectiveTTL}),
            entry(Start, TTL, Key, Value);
        eject ->
            error_logger:info_msg("Key ~p ejected~n", [Key]),
            ok
    end.

random_string(Length) ->
    [$` + random:uniform(26) || _ <- lists:seq(1, Length)].

sync_caches(Seed, Cache) ->
    Keys = cache:keys(Seed),
    io:format("Seeding ~p keys~n", [length(Keys)]),
    insert_keys(Keys, Cache, Seed).

insert_keys([], _Cache, _Seed) ->
    ok;
insert_keys([Key|T], Cache, Seed) ->
    case cache:get(Seed, Key) of
        {ok, Value, TTL} ->
            error_logger:info_msg("Synced key ~p~n", [Key]),
            cache:put(Key, Value, TTL);
        _ ->
            error_logger:info_msg("Skipping key ~p~n", [Key])
    end,
    insert_keys(T, Cache, Seed).
