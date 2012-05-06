-module(uptime).

-behaviour(gen_event).

-export([ init/1
        , handle_event/2
        , handle_call/2
        , handle_info/2
        , terminate/2
        , code_change/3
    ]).

-export([uptime/1]).

-include("irc.hrl").

init({{_EventMgr, _Irc} = Args, _Term}) ->
    %% init as result of swap_handler
    init(Args);
init({EventMgr, #irc_state{network = Network} = _Irc}) ->
    {ok, #eiko_plugin{
            name = uptime,
            event = EventMgr,
            commands = [
                #command{
                    event = <<"PRIVMSG">>, match = {cmd, <<"uptime">>},
                    function = {?MODULE, uptime}, args = [],
                    usage = "vm uptime"}
            ]}
    }.

handle_event({in, Irc, Msg}, State) ->
    eiko_plugin:handle({Irc, Msg}, State),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) -> {ok, noreply, State}.
handle_info(_Info, State) -> {ok, State}.
terminate(_Args, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

uptime({Irc, Msg}) ->
    eiko_log:info({?IRCNET(Irc), self()}, "Called by ~p", [eiko_lib:get_origin(Msg)]),
    {Total, _} = statistics(wall_clock),
    {D, {H, M, _S}} = calendar:seconds_to_daystime(Total div 1000),
    Uptime = lists:flatten(io_lib:format("~p d, ~p:~p", [D, H, M])),
    eiko_lib:reply(Irc, Msg, Uptime).

