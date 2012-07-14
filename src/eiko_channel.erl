-module(eiko_channel).
-behaviour(gen_event).

-export([ init/1
        , handle_event/2
        , handle_call/2
        , handle_info/2
        , terminate/2
        , code_change/3

        , join_all/1
    ]).

-include("irc.hrl").

init({{_EventMgr, _Irc} = Args, _Term}) ->
    init(Args);
init({EventMgr, #irc_state{network = Network} = _Irc}) ->
    {ok, #eiko_plugin{
            name = channel,
            event = EventMgr,
            commands = [
                #command{
                    event = <<"001">>, match = all,
                    function = {?MODULE, join_all}, args = ignore,
                    usage = "JOINs channel at startup"}
            ]}
    }.

handle_event({in, Irc, Msg}, State) ->
    eiko_plugin:handle({Irc, Msg, State}, State),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) -> {ok, noreply, State}.
handle_info(_Info, State) -> {ok, State}.
terminate(_Args, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


join_all({Irc, Msg, _State}) ->
    Network = Irc#irc_state.network,
    Channels = eiko_cfg:channels(Network),
    lists:foreach(
        fun(C) ->
                Props = {kvc:value(name, C, undefined), kvc:value(autojoin, C, false)},
                case Props of
                    {undefined, _} ->
                        eiko_log:error({Network, server}, "Channel missing name: ~p", [C]);
                    {Name, true} ->
                        eiko_log:add_channel(Network, Name),
                        eiko_network:join(Irc, Name);
                    {_, _} -> ok
                end
        end, Channels).

