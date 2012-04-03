-module(irc_config).
-behaviour(gen_server).

%%% Callbacks
-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
    ]).

%%% API
-export([
        start_link/1,
        networks/0,
        network/1,
        nick/1,
        user/1,
        servers/1,
        channels/1,
        reload/0
    ]).

-record(state, {
        filename,
        config
    }).

-compile({no_auto_import, [get/1]}).

start_link(Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #state{filename = Filename}, []).

networks() -> get(networks).
network(Network) -> get([networks, Network]).

nick(Network) -> get([networks, Network, nick]).
user(Network) ->
    case get([networks, Network, user]) of
        [] -> nick(Network);
        User -> User
    end.

servers(Network) -> get([networks, Network, servers]).
channels(Network) -> get([networks, Network, channels]).

reload() -> gen_server:call(?MODULE, reload).


%%%--------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------
init(#state{filename=Filename} = State) ->
    case load(Filename) of
        {ok, Terms} -> {ok, State#state{config=Terms}};
        {error, Reason} -> {stop, {error, Reason}}
    end.

handle_call({get, Key}, _From, #state{config=Config} = State) ->
    {reply, kvc:path(Key, Config), State};
handle_call(reload, _From, #state{filename=Filename} = State) ->
    case load(Filename) of
        {ok, Terms} -> {reply, ok, State#state{config=Terms}};
        {error, Reason} ->
            % Error in config, keep using old one. Maybe we shouldn't?
            {reply, {error, Reason}, State}
    end.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%--------------------------------------------------
%%% Internal
%%%--------------------------------------------------
get(Key) -> gen_server:call(?MODULE, {get, Key}).

load(Filename) ->
    case file:consult(Filename) of
        {ok, Terms} -> {ok, Terms};
        {error, Reason} -> {error, file:format_error(Reason)}
    end.

