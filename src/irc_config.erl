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
        start_link/0,
        networks/0,
        network/1,
        nick/1,
        user/1,
        servers/1,
        channels/1,
        plugins/1
    ]).

-record(state, {
        config
    }).

-compile({no_auto_import, [get/1]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

networks() -> get(networks).
network(Network) -> get([networks, Network]).

nick(Network) -> 
    irc_util:to_binary( get([networks, Network, nick]) ).
user(Network) ->
    case get([networks, Network, user]) of
        [] -> nick(Network);
        User -> irc_util:to_binary(User)
    end.

servers(Network) -> get([networks, Network, servers]).
channels(Network) -> get([networks, Network, channels]).

plugins(Network) -> get([networks, Network, plugins]).


%%%--------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------
init(_) ->
    {ok, #state{config = application:get_all_env(eiko_core)}}.

handle_call({get, Key}, _From, #state{config=Config} = State) ->
    {reply, kvc:path(Key, Config), State}.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%--------------------------------------------------
%%% Internal
%%%--------------------------------------------------
get(Key) -> gen_server:call(?MODULE, {get, Key}).

