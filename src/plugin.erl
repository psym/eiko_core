-module(plugin).
-behaviour(gen_event).

-compile([{parse_transform, lager_transform}]).

-export([ init/1
        , handle_event/2
        , handle_call/2
        , handle_info/2
        , terminate/2
        , code_change/3
    ]).

-export([ load/2
        , unload/2
        , loaded/1
    ]).

-include("irc.hrl").

init({{_EventMgr, _Irc} = Args, _Term}) ->
    %% init as result of swap_handler
    init(Args);
init({EventMgr, #irc_state{network = Network} = _Irc}) ->
    eiko_log:add_plugin(Network, "plugin"),
    {ok, #eiko_plugin{
            name = plugin,
            event = EventMgr,
            commands = [
                #command{
                    event = <<"PRIVMSG">>, match = {cmd, <<"!load">>},
                    function = {?MODULE, load}, args = [something],
                    usage = "<plugin> -- load or reload plugin"},
                #command{
                    event = <<"PRIVMSG">>, match = {cmd, <<"!unload">>},
                    function = {?MODULE, unload}, args = [something],
                    usage = "<plugin> -- unload plugin"},
                #command{
                    event = <<"PRIVMSG">>, match = {cmd, <<"!loaded">>},
                    function = {?MODULE, loaded}, args = [],
                    usage = "list loaded plugins"}
            ]}
    };
%%% swap_handler
init({Args, _Term}) -> init(Args).

handle_event({in, Irc, Msg}, State) ->
    eiko_plugin:handle({Irc, Msg}, State),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_Reg, State) -> {ok, noreply, State}.
handle_info(_Info, State) -> {ok, State}.
terminate(_Args, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


unload({Irc, Msg}, Plugin) ->
    EventMgr = Irc#irc_state.event,
    Loaded = eiko_plugin:which_handlers(EventMgr),
    P = eiko_util:normalize(Plugin, atom),

    io:format("loaded: ~p~n", [Loaded]),
    io:format("unloading ~p~n", [Plugin]),
    io:format("~p located at ~p~n", [P, code:which(P)]),

    case lists:member(P, Loaded) of
        false ->
            eiko_lib:reply(Irc, Msg, io_lib:format("'~p' isn't loaded", [P]));
        true ->
            Status = eiko_plugin:delete_handler(EventMgr, P, []),
            io:format("~p~n", [Status]),
            eiko_lib:reply(Irc, Msg, io_lib:format("Unloading '~p'...~p", [P, Status]))
    end,
    ok.

loaded({Irc, Msg}) ->
    EventMgr = Irc#irc_state.event,
    Loaded = eiko_plugin:which_handlers(EventMgr),
    L = [eiko_util:normalize(P, string) || P <- Loaded],
    eiko_lib:reply(Irc, Msg, io_lib:format("Loaded plugins: ~ts", [string:join(L, ", ")])).

load({Irc, Msg}, Plugin) ->
    EventMgr = Irc#irc_state.event,
    case eiko_access:is_owner(Msg) of
        false ->
            eiko_lib:notice(Irc, Msg, "you are not owner", private);
        true ->
            eiko_lib:notice(Irc, Msg, "yes master", private)
    end,
    Loaded = eiko_plugin:which_handlers(EventMgr),
    P = eiko_util:normalize(Plugin, atom),

    io:format("loaded: ~p~n", [Loaded]),
    io:format("loading: ~p~n", [Plugin]),
    io:format("~p located at ~p~n", [P, code:which(P)]),

    case code:which(P) of
        non_existing -> 
            eiko_lib:reply(Irc, Msg, io_lib:format("Module '~p' does not exist", [P]));
        _ -> 
            case lists:member(P, Loaded) of 
                false -> load_new({Irc, Msg}, P, EventMgr);
                true -> reload({Irc, Msg}, P, EventMgr)
            end
    end.

load_new({Irc, Msg}, Plugin, EventMgr) ->
    Status = eiko_plugin:add_handler(EventMgr, Plugin, Irc),
    eiko_lib:reply(Irc, Msg, io_lib:format("Loading '~p'...~p", [Plugin, Status])).

reload({Irc, Msg}, Plugin, EventMgr) ->
    code:purge(Plugin),
    case code:load_file(Plugin) of
        {module, Module} -> io:format("Reloaded ~p module (~p).~n", [Plugin, Module]);
        {error, _Error} -> io:format("Reload ~p module failed.~n", [Plugin])
    end,
    Status = case eiko_plugin:swap_handler(EventMgr, {Plugin, []}, {Plugin, Irc}) of
        {error, R} -> 
            eiko_log:error([{?IRCNET(Irc), server}], "Reloading ~p failed: ~p", [Plugin, R]),
            error;
        ok -> ok
    end,
    eiko_lib:reply(Irc, Msg, io_lib:format("Reloading '~p'...~p", [Plugin, Status])).

