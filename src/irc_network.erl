-module(irc_network).
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
        trace,

        nick
    }).

start_link(Network) -> gen_fsm:start_link(?MODULE, Network, []).

%%% gen_fsm API callbacks
init(Network) ->
    lager:info("Initializing network '~s'", [Network]),
    TraceFile = io_lib:format("log/~s/server.log", [Network]),
    {ok, Trace} = lager:trace_file(TraceFile, [{network, Network}]),
    State = #state{
        network = Network, 
        trace   = Trace,
        nick    = irc_config:nick(Network)
    },
    Settings = irc_config:network(Network),
    case kvc:path(autoconnect,Settings) of
        true -> {ok, connect, State, 0};
        _ -> {ok, idle, State, hibernate}
    end.

connect(timeout, #state{network=Network} = State) ->
    {Host, Port} = hd(irc_config:servers(Network)),
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
    Msg = irc_lib:parse(Line),
    lager:debug([{network, State#state.network}], "in: ~ts", [Line]),
    %XXX: forward to plugins
    {next_state, online, handle_line(Msg, State)}.

offline(_, #state{network = Network} = State) ->
    lager:info([{network, Network}], "Network '~s' offline", [Network]),
    {next_state, offline, State}.

handle_event({out, [H|T]}, StateName, #state{socket=Socket, network=Network} = State) ->
    %XXX: this can be improved
    L = [H | [list_to_binary([$\s | X]) || X <- T]],
    lager:debug([{network, Network}], "out: ~ts", [L]),
    ok = gen_tcp:send(Socket, [L, ?CRLF]),
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(Reason, _StateName, State) ->
    gen_tcp:close(State#state.socket),
    lager:stop_trace(State#state.trace),
    %XXX: push config changes back
    {shutdown, Reason}.


%%%--------------------------------------------------
%%% Internal
%%%--------------------------------------------------
handle_line(Msg = #irc_message{command = <<"PING">>}, State) ->
    irc_lib:send(self(), <<"PONG">>, Msg#irc_message.trailing),
    State;
handle_line(#irc_message{command = <<"001">>}, State) -> % RPL_WELCOME
    join_channels(State);
handle_line(#irc_message{command = <<"433">>}, State) -> % ERR_NICKNAMEINUSE
    NewNick = <<(State#state.nick)/binary, "`">>,
    irc_lib:nick(self(), NewNick),
    State#state{nick = NewNick};
handle_line(Msg, #state{network = Network} = State) ->
    lager:debug([{network, Network}], "unhandled: ~p", [Msg]),
    State.

login(Network) -> 
    Nick = irc_config:nick(Network),
    User = irc_config:user(Network),
    irc_lib:nick(self(), Nick),
    irc_lib:user(self(), User, User).

join_channels(#state{network = Network} = State) ->
    Channels = irc_config:channels(local),
    lists:foreach(
        fun(C) ->
                Props = {kvc:value(name, C, undefined), kvc:value(autojoin, C, false)},
                case Props of
                    {undefined, _} ->
                        lager:error([{network, Network}], "Channel missing name: ~p", [C]);
                    {Name, true} ->
                        irc_lib:join(self(), Name);
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


