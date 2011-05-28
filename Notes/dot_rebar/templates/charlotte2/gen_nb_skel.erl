-module({{name}}).

-behaviour(gen_nb_server).

%% API
-export([start_link/0,
         add_port/1,
         del_port/1]).

%% gen_server callbacks
-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% gen_nb_server callbacks
-export([init/2,
         new_connection/4,
         sock_opts/0]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_nb_server:start_link({local, ?SERVER}, ?MODULE, []).

add_port(Port) when is_integer(Port) ->
    gen_server:call(?SERVER, {add_port, {"127.0.0.1", Port}}).

del_port(Port) when is_integer(Port) ->
    gen_server:call(?SERVER, {del_port, {"127.0.0.1", Port}}).

init([], State) ->
    case gen_nb_server:add_listen_socket({"127.0.0.1", 9000}, State) of
        {ok, State1} ->
            {ok, State1};
        Error ->
            {stop, Error}
    end.

handle_call({add_port, AddrPort}, _From, State) ->
    case gen_nb_server:add_listen_socket(AddrPort, State) of
        {ok, State1} ->
            {reply, ok, State1};
        Error ->
            {reply, Error, State}
    end;
handle_call({del_port, AddrPort}, _From, State) ->
    case gen_nb_server:remove_listen_socket(AddrPort, State) of
        {ok, State1} ->
            {reply, ok, State1};
        Error ->
            {reply, Error, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sock_opts() ->
    [].

new_connection(IpAddr, Port, Sock, State) ->
    {ok, State}.
