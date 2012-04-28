-module(eiko_network).
-behaviour(gen_fsm).

-compile([{parse_transform, lager_transform}]).

-include("irc.hrl").

%%% API
-export([start_link/1]).

%%% Callbacks
-export([
        init/1,
        handle_info/3, 
        handle_event/3,
        code_change/4,
        terminate/3,
        %%% States
        offline/2,
        connect/2,
        online/2,
        disconnect/2
    ]).

-define(RECONNECT_DELAY, 10 * 1000).        % 10 seconds
-define(PING_INTERVAL, 120 * 1000).         % 120 seconds
-define(CRLF, <<"\r\n">>).

-record(state, {
        network,
        socket,
        irc     :: #irc_state{}
    }).


start_link(Network) -> gen_fsm:start_link(?MODULE, Network, []).

%%% gen_fsm API callbacks
init(Network) ->
    lager:info("Initializing network '~s'", [Network]),
    eiko_log:add_network(Network),
    State = #state{
        network = Network, 
        irc     = #irc_state{
                        nick = eiko_cfg:nick(Network),
                        ref  = self()
                    }
    },
    case eiko_cfg:network(Network, autoconnect) of
        true -> {ok, connect, State, 0};
        _ -> {ok, idle, State, hibernate}
    end.

connect(timeout, #state{network=Network} = State) ->
    {Host, Port} = hd(eiko_cfg:servers(Network)),
    Options = [binary, {active, true}, {packet, line}, {keepalive, true}],
    case gen_tcp:connect(Host, Port, Options) of
        {ok, Socket} ->
            login(Network),
            {next_state, online, State#state{socket=Socket}};
        {error, Reason} ->
            %XXX: handle reconnects
            {stop, {error, Reason}, State}
    end.

disconnect(_, State) ->
    {stop, unimplemented, State}.

online({in, Line}, State) ->
    Msg = eiko_lib:parse(Line),
    eiko_log:log_msg(State#state.network, in, Msg),
    %XXX: forward to plugins
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
handle_line(Msg = #irc_message{command = <<"PING">>}, State) ->
    eiko_lib:send(self(), <<"PONG">>, Msg#irc_message.trailing),
    State;
handle_line(#irc_message{command = <<"001">>}, State) -> % RPL_WELCOME
    join_channels(State);
handle_line(Msg, #state{irc = Irc} = State) when
        Msg#irc_message.command == <<"433">>;       % ERR_NICKNAMEINUSE
        Msg#irc_message.command == <<"436">> ->     % ERR_NICKCOLLISION
    NewNick = Irc#irc_state.nick ++ "`",
    eiko_lib:nick(self(), NewNick),
    State#state{irc = Irc#irc_state{nick = NewNick}};
handle_line(Msg, #state{network = Network} = State) ->
    eiko_log:debug({Network, server}, "unhandled: ~p", [Msg]),
    State.

login(Network) -> 
    Nick = eiko_cfg:nick(Network),
    User = eiko_cfg:user(Network),
    eiko_lib:nick(self(), Nick),
    eiko_lib:user(self(), User, User).

join_channels(#state{network = Network} = State) ->
    Channels = eiko_cfg:channels(local),
    lists:foreach(
        fun(C) ->
                Props = {kvc:value(name, C, undefined), kvc:value(autojoin, C, false)},
                case Props of
                    {undefined, _} ->
                        eiko_log:error({Network, server}, "Channel missing name: ~p", [C]);
                    {Name, true} ->
                        eiko_log:add_channel(Network, Name),
                        eiko_lib:join(self(), Name);
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


