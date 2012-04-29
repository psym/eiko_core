-module(eiko_network).
-behaviour(gen_fsm).

-compile([{parse_transform, lager_transform}]).

-include("irc.hrl").

%%% API
-export([ start_link/1
        , nick/2
        , join/2
        , send/3
    ]).

%%% Callbacks
-export([ init/1
        , handle_info/3 
        , handle_event/3
        , code_change/4
        , terminate/3
        
        %%% States
        , offline/2
        , connect/2
        , online/2
        , disconnect/2
    ]).

-define(RECONNECT_DELAY, 10 * 1000).        % 10 seconds
-define(PING_INTERVAL, 120 * 1000).         % 120 seconds
-define(CRLF, <<"\r\n">>).

-record(state, {
        network     :: atom(),
        socket,
        irc         :: #irc_state{},
        event       :: pid()
    }).


%%%--------------------------------------------------
%%% API
%%%--------------------------------------------------
start_link(Network) -> 
    gen_fsm:start_link(?MODULE, Network, []).

nick(Irc, Nick) when is_record(Irc, irc_state) ->
    send(Irc, "NICK", Nick).

join(Irc, Channel) when is_record(Irc, irc_state) ->
    send(Irc, "JOIN", Channel).

send(_, _, []) -> ok;
send(#irc_state{ref= Ref}, Cmd, Trailing) ->
    D = eiko_util:normalize(Trailing, binary),
    Lim = 510 - iolist_size(Cmd) - 2,
    Size = iolist_size(D),
    case min(Lim, Size) of
        Lim ->
            ToSend = binary:part(D, 0, Lim),
            Rest = binary:part(D, Lim, Size-Lim);
        Size ->
            ToSend = D,
            Rest = []
    end,
    gen_fsm:send_all_state_event(Ref, {out, [Cmd | [<<$:, ToSend/binary>>]]}),
    send(Ref, Cmd, Rest).


%%%--------------------------------------------------
%%% gen_fsm API callbacks
%%%--------------------------------------------------
init(Network) ->
    lager:info("Initializing network '~s'", [Network]),
    eiko_log:add_network(Network),
    Irc = #irc_state{
        network = Network,
        nick    = eiko_cfg:nick(Network),
        ref     = self()
    },
    {ok, EventMgr} = eiko_plugin:start_link(Irc),
    State = #state{
        network = Network, 
        irc     = Irc,
        event   = EventMgr
    },
    case eiko_cfg:network(Network, autoconnect) of
        true -> {ok, connect, State, 0};
        _ -> {ok, idle, State, hibernate}
    end.

connect(timeout, #state{network=Network, irc=Irc} = State) ->
    {Host, Port} = hd(eiko_cfg:servers(Network)),
    Options = [binary, {active, true}, {packet, line}, {keepalive, true}],
    case gen_tcp:connect(Host, Port, Options) of
        {ok, Socket} ->
            login(Irc, Network),
            {next_state, online, State#state{socket=Socket}};
        {error, Reason} ->
            %XXX: handle reconnects
            {stop, {error, Reason}, State}
    end.

disconnect(_, State) ->
    {stop, unimplemented, State}.

online({in, Line}, #state{irc = Irc, event = Event} = State) ->
    Msg = eiko_lib:parse(Line),
    eiko_log:log_msg(State#state.network, in, Msg),
    eiko_plugin:notify(Event, {in, Irc, Msg}),
    {next_state, online, handle_line(Msg, State)}.


offline(_, #state{network = Network} = State) ->
    eiko_log:info({Network, server}, "Network '~s' offline", [Network]),
    {next_state, offline, State}.

handle_event({out, [H|T]}, StateName, #state{socket=Socket, network=Network} = State) ->
    %XXX: this can be improved
    L = [H | [list_to_binary([$\s | X]) || X <- T]],
    eiko_log:log_msg(Network, out, L),
    ok = gen_tcp:send(Socket, [L, ?CRLF]),
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(Reason, _StateName, State) ->
    gen_tcp:close(State#state.socket),
    %XXX: push config changes back
    {shutdown, Reason}.


%%%--------------------------------------------------
%%% Internal
%%%--------------------------------------------------
handle_line(Msg = #irc_message{command = <<"PING">>}, #state{irc = Irc} = State) ->
    send(Irc, <<"PONG">>, Msg#irc_message.trailing),
    State;
handle_line(#irc_message{command = <<"001">>}, State) -> % RPL_WELCOME
    join_channels(State);
handle_line(Msg, #state{irc = Irc} = State) when
        Msg#irc_message.command == <<"433">>;       % ERR_NICKNAMEINUSE
        Msg#irc_message.command == <<"436">> ->     % ERR_NICKCOLLISION
    NewNick = Irc#irc_state.nick ++ "`",
    nick(Irc, NewNick),
    State#state{irc = Irc#irc_state{nick = NewNick}};
handle_line(Msg, #state{network = Network} = State) ->
    eiko_log:debug({Network, server}, "unhandled: ~p", [Msg]),
    State.

login(Irc, Network) -> 
    Nick = eiko_cfg:nick(Network),
    User = eiko_cfg:user(Network),
    nick(Irc, Nick),
    user(Irc, User, User).

user(Irc, User, RealName) ->
    send(Irc, ["USER ", User, " 0 * "], [RealName]).

join_channels(#state{network = Network, irc = Irc} = State) ->
    Channels = eiko_cfg:channels(Network),
    lists:foreach(
        fun(C) ->
                Props = {kvc:value(name, C, undefined), kvc:value(autojoin, C, false)},
                case Props of
                    {undefined, _} ->
                        eiko_log:error({Network, server}, "Channel missing name: ~p", [C]);
                    {Name, true} ->
                        eiko_log:add_channel(Network, Name),
                        join(Irc, Name);
                    {_, _} -> ok
                end
        end, Channels),
    State.


%%%--------------------------------------------------
%%% TCP Handling
%%%--------------------------------------------------
handle_info({tcp, _Socket, Data}, online, State) ->
    [Line, <<>>] = binary:split(Data, ?CRLF),
    gen_fsm:send_event(self(), {in, Line}),
    {next_state, online, State};
handle_info({tcp_closed, _Socket}, _StateName, State) ->
    %XXX: handle reconnect
    {stop, disconnect, State}.


