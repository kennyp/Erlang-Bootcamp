-module(cache_fe).

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

-record(state, {caches=[]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
    cache_fe:put(Key, Value, ?DEFAULT_TTL).

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
    [cache_fe:put(random_string(5), random_string(3)) || _ <- lists:seq(1, 100)],
    ok.

init([]) ->
    process_flag(trap_exit, true),
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3),
    {ok, #state{}}.

handle_call({register_cache, Cache}, _From, #state{caches=[]}=State) ->
    {reply, ok, State#state{caches=[Cache]}};
handle_call({register_cache, Cache}, _From, #state{caches=Caches}=State) ->
    SeedCache = pick_random_cache(Caches),
    error_logger:info_msg("Seeding new cache ~p from existing cache ~p~n", [Cache, SeedCache]),
    {reply, {sync, SeedCache}, State#state{caches=[Cache|Caches]}};
handle_call(Msg, _From, #state{caches=[]}=State) ->
    {reply, {error, no_backends}, State};
handle_call(Msg, _From, #state{caches=Caches}=State) ->
    Cache = pick_random_cache(Caches),
    {reply, gen_server:call(Cache, Msg), State}.

handle_cast(crash, State) ->
    exit(self(), kill),
    {noreply, State};
handle_cast(Msg, #state{caches=[]}=State) ->
    {noreply, State};
handle_cast(Msg, #state{caches=Caches}=State) ->
    multi_cast(Caches, Msg),
    {noreply, State}.

handle_info({'EXIT', Cache, _Reason}, #state{caches=Caches}=State) ->
    {noreply, State#state{caches=lists:delete(Cache, Caches)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
random_string(Length) ->
    [$` + random:uniform(26) || _ <- lists:seq(1, Length)].

pick_random_cache(Caches) ->
    Pos = random:uniform(length(Caches)),
    lists:nth(Pos, Caches).

multi_cast(Caches, Msg) ->
    [gen_server:cast(Cache, Msg) || Cache <- Caches].
