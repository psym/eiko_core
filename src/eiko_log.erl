-module(eiko_log).
-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-export([start_link/0]).

%%% API
-export(
        %% Registration functions
        [ list/0
        , add_network/1
        , add_channel/2
        , add_plugin/2
        , remove_network/1
        , remove_channel/2
        , remove_plugin/2

        , add_trace/2
        , remove_trace/1

        %% Basic logging functions
        , debug/3
        , info/3
        , notice/3
        , warning/3
        , error/3
        , critical/3
        , alert/3
        , emergency/3

        %% Complex, specific logging functions
        , log_msg/3
    ]).

%%% Callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
    ]).

-include("irc.hrl").


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%--------------------------------------------------
%%% API
%%%--------------------------------------------------

log_msg(Network, Direction, #irc_message{command = Cmd, params = Params} = Msg) ->
    if
        Cmd == <<"PRIVMSG">> ->
            eiko_log:info({Network, hd(Params)}, "~p: ~p", [Direction, Msg]);
        true ->
            eiko_log:info({Network, server}, "~p: ~p", [Direction, Msg])
    end;
log_msg(Network, Direction, Line) ->
    log_msg(Network, Direction, eiko_lib:parse(Line)).


debug({Network, Plugin}, Msg, Args) when is_pid(Plugin) ->
    lager:debug([{network, bin(Network)}, {plugin, bin(Plugin)}], Msg, Args);
debug({Network, Channel}, Msg, Args) ->
    lager:debug([{network, bin(Network)}, {channel, bin(Channel)}], Msg, Args).

info({Network, Plugin}, Msg, Args) when is_pid(Plugin) ->
    lager:info([{network, bin(Network)}, {plugin, bin(Plugin)}], Msg, Args);
info({Network, Channel}, Msg, Args) ->
    lager:info([{network, bin(Network)}, {channel, bin(Channel)}], Msg, Args).

notice({Network, Plugin}, Msg, Args) when is_pid(Plugin) ->
    lager:notice([{network, bin(Network)}, {plugin, bin(Plugin)}], Msg, Args);
notice({Network, Channel}, Msg, Args) ->
    lager:notice([{network, bin(Network)}, {channel, bin(Channel)}], Msg, Args).

warning({Network, Plugin}, Msg, Args) when is_pid(Plugin) ->
    lager:warning([{network, bin(Network)}, {plugin, bin(Plugin)}], Msg, Args);
warning({Network, Channel}, Msg, Args) ->
    lager:warning([{network, bin(Network)}, {channel, bin(Channel)}], Msg, Args).

error({Network, Plugin}, Msg, Args) when is_pid(Plugin) ->
    lager:error([{network, bin(Network)}, {plugin, bin(Plugin)}], Msg, Args);
error({Network, Channel}, Msg, Args) ->
    lager:error([{network, bin(Network)}, {channel, bin(Channel)}], Msg, Args).

alert({Network, Plugin}, Msg, Args) when is_pid(Plugin) ->
    lager:alert([{network, bin(Network)}, {plugin, bin(Plugin)}], Msg, Args);
alert({Network, Channel}, Msg, Args) ->
    lager:alert([{network, bin(Network)}, {channel, bin(Channel)}], Msg, Args).

critical({Network, Plugin}, Msg, Args) when is_pid(Plugin) ->
    lager:critical([{network, bin(Network)}, {plugin, bin(Plugin)}], Msg, Args);
critical({Network, Channel}, Msg, Args) ->
    lager:critical([{network, bin(Network)}, {channel, bin(Channel)}], Msg, Args).

emergency({Network, Plugin}, Msg, Args) when is_pid(Plugin) ->
    lager:emergency([{network, bin(Network)}, {plugin, bin(Plugin)}], Msg, Args);
emergency({Network, Channel}, Msg, Args) ->
    lager:emergency([{network, bin(Network)}, {channel, bin(Channel)}], Msg, Args).


list() ->
    gen_server:cast(?MODULE, list).

add_network(Network) ->
    add_channel(Network, server).

remove_network(Network) ->
    remove_channel(Network, server).

add_channel(Network, Channel) ->
    File = io_lib:format("log/~s/~s.log", [Network, Channel]),
    add_trace(File, [{network, bin(Network)}, {channel, bin(Channel)}]).

remove_channel(Network, Channel) ->
    remove_trace([{network, bin(Network)}, {channel, bin(Channel)}]).

add_plugin(Network, Plugin) ->
    File = io_lib:format("log/~s/plugins/~s.log", [Network, Plugin]),
    add_trace(File, [{network, bin(Network)}, {plugin, bin(Plugin)}]).

remove_plugin(Network, Plugin) ->
    remove_trace([{network, bin(Network)}, {plugin, bin(Plugin)}]).

add_trace(File, Filter) ->
    gen_server:call(?MODULE, {add_trace, File, Filter}).

remove_trace(Trace) ->
    gen_server:call(?MODULE, {remove_trace, Trace}).


%%%--------------------------------------------------
%%% Callbacks
%%%--------------------------------------------------
init(_) ->
    process_flag(trap_exit, true),
    {ok, dict:new()}.

handle_call({add_trace, File, Filter}, {Pid, _Tag}, State) ->
    case lager:trace_file(File, Filter) of
        {ok, Trace} ->
            case dict:find(Pid, State) of
                {ok, _} -> ok;
                error   -> monitor(process, Pid)
            end,
            {reply, ok, dict:append(Pid, {Trace, Filter}, State)};
        Error ->
            lager:critical("Add trace failed: ~p. File: (~p) Filter: (~p)", [Error, File, Filter]),
            {reply, Error, State}
    end;
handle_call({remove_trace, Filter}, {Pid, _Tag}, State) ->
    F = lists:sort(Filter),
    NewState = lists:filter( 
        fun({T, TF}) ->
            case F == lists:sort(TF) of
                true ->
                    lager:stop_trace(T),
                    false;
                false -> true
            end
        end, dict:fetch(Pid, State)),
    {reply, ok, NewState}.

handle_cast(list, State) ->
    io:format("~p~n", [State]),
    {noreply, State};
handle_cast(_Request, State) -> {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) -> 
    {noreply, remove_pid(Object, State)};
handle_info({'EXIT', Pid, _Info}, State) ->
    {noreply, remove_pid(Pid, State)}.

terminate(_Reason, State) ->
    [ [lager:stop_trace(Trace) || {Trace, _} <- dict:fetch(Pid, State)]
      || Pid <- dict:fetch_keys(State) ],
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%--------------------------------------------------
%%% Internal
%%%--------------------------------------------------
remove_pid(Pid, State) ->
    case dict:is_key(Pid, State) of
        true ->
            io:format("removing traces for ~p: ~p~n", [Pid, dict:fetch(Pid, State)]),
            [lager:stop_trace(Trace) || {Trace, _} <- dict:fetch(Pid, State)],
            dict:erase(Pid, State);
        _ -> State
    end.

bin(X) -> eiko_util:normalize(X, binary).

