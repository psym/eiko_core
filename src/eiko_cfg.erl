-module(eiko_cfg).
-behaviour(gen_server).

%%% API
-export([ start_link/0
        , get/0
        , get/1
        , first_of/1
        , set/2
        , reload/0
        , stop/0

        , networks/0
        , network/1
        , network/2
        , nick/1
        , user/1
        , owners/0
        , servers/1
        , channels/1
        , plugins/1
        , plugin_options/1
        , plugin_access/1
        , plugin_prefix/1
        , prefix/1
    ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
    ]).

-record(state, {
        config = []
    }).

-compile({no_auto_import, [get/1]}).

%%%--------------------------------------------------
%%% API
%%%--------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get() ->
    get("").

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
    gen_server:cast(?MODULE, {set, Key, Value}).

reload() ->
    gen_server:cast(?MODULE, reload).

stop() ->
    gen_server:cast(?MODULE, stop).


networks() ->
    get(networks).

network(Network) -> 
    get([networks, Network]).

network(Network, Extra) ->
    get([networks, Network] ++ Extra).

nick(Network) ->
    first_of([[networks, Network, nick], nick]).

user(Network) ->
    case first_of([[networks, Network, user], user]) of
        [] -> nick(Network);
        U  -> U
    end.

owners() ->
    [eiko_util:normalize(X, binary) || X <- get(owners)].

servers(Network) ->
    get([networks, Network, servers]).

channels(Network) ->
    get([networks, Network, channels]).

plugins(Network) ->
    get([networks, Network, plugins]).

plugin_options(Plugin) ->
    get([plugins, Plugin]).

%% default command access is owner only
plugin_access(Plugin) ->
    % kvc:value ingores default value if store is empty
    case kvc:path(access, plugin_options(Plugin)) of
        [] -> owner;
        A  -> A
    end.

plugin_prefix(Plugin) ->
    case kvc:path(prefix, plugin_options(Plugin)) of
        [] ->
            case get(prefix) of
                [] -> direct;
                DP -> DP
            end;
        P  -> P
    end.

prefix(Network) ->
    first_of([[networks, Network, prefix], prefix]).

first_of([]) -> [];
first_of([H|T]) ->
    case get(H) of
        [] -> first_of(T);
        X  -> X
    end.


%%%--------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------
init(_) ->
    {ok, #state{config = ensure_file()}}.

handle_call({get, Key}, _From, #state{config=Config} = State) ->
    {reply, kvc:path(Key, Config), State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(reload, State) ->
    {noreply, State#state{config = ensure_file()}};
handle_cast({set, Key, Value}, #state{config=Config} = State) ->
    NewConfig = do_set(Key, Value, Config),
    {noreply, State#state{config = write_file(NewConfig)}}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%--------------------------------------------------
%%% Internal
%%%--------------------------------------------------
filename() ->
    {ok, File} = application:get_env(eiko_core, config),
    File.

ensure_file() ->
    case read_file() of
        {ok, C} -> C;
        {error, _} -> []
    end.

read_file() ->
    File = filename(),
    case file:consult(File) of
        {ok, Config} -> 
            {ok, Config};
        {error, Reason} ->
            lager:error("failed to load ~s: ~s", [File, file:format_error(Reason)]),
            {error, Reason}
    end.

write_file(Config) ->
    {ok, FD} = file:open(filename(), [write]),
    try
        Data = [io_lib:format("~p.~n", [E]) || E <-Config],
        ok = file:write(FD, iolist_to_binary(Data))
    after
        ok = file:close(FD)
    end,
    Config.


do_set(K, V, C) ->
    P = [eiko_util:normalize(X, atom) || X <- path(K)],
    do_set1(P, V, C).
do_set1([H|[]], V, C) ->
    lists:keystore(H, 1, C, {H, V});
do_set1([H|T], V, C) ->
    lists:keystore(H, 1, C, {H, 
            case lists:keyfind(H, 1, C) of 
                {_, C1} when is_list(C1)->
                    do_set1(T, V, C1);
                _ -> 
                    do_set1(T, V, [])
            end}).

path(K) ->
    binary:split(eiko_util:normalize(K, binary), <<".">>, [global]).


