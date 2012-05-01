-module(eiko_plugin).

-compile([{parse_transform, lager_transform}]).

-export([ 
        strip/1
    ]).

%%% Callbacks
-export([
        start_link/1,
        add_handler/3,
        delete_handler/3,
        which_handlers/1,
        swap_handler/3,
        notify/2
    ]).

-compile(export_all).

-include("irc.hrl").

%%%--------------------------------------------------
%%%  API
%%%--------------------------------------------------
start_link(Irc) when is_record(Irc, irc_state)->
    Network = Irc#irc_state.network,
    {ok, EventMgr} = gen_event:start_link(),
    lager:info("Starting event manager for '~s': ~p", [Network, EventMgr]),
    [add_handler(EventMgr, Plugin, [Irc]) || Plugin <- eiko_cfg:plugins(Network)],
    {ok, EventMgr}.

add_handler(EventMgr, Handler, _Args) ->
    case gen_event:add_handler(EventMgr, Handler, [EventMgr]) of
        ok -> 
            lager:info("Loading '~p' on ~p ... ok", [Handler, EventMgr]);
        {E, Reason} when E == 'EXIT'; E == error ->
            lager:error("Loading '~p' on ~p ... failed (~p)", [Handler, EventMgr, Reason])
    end.

delete_handler(EventMgr, Handler, Args) ->
    gen_event:delete_handler(EventMgr, Handler, Args).

which_handlers(EventMgr) ->
    gen_event:which_handlers(EventMgr).

swap_handler(EventMgr, {OldHandler, TermArgs}, {NewHandler, _Args}) ->
    gen_event:swap_handler(EventMgr, {OldHandler, TermArgs}, {NewHandler, [EventMgr]}).

notify(EventMgr, Event) ->
    gen_event:notify(EventMgr, Event).


command_match(_Msg, #command{match = all}) -> 
    true;
command_match(_Msg, #command{match = {cmd, <<>>}}) -> 
    true;
command_match(Msg, #command{match = {func, Match}}) ->
    Match(Msg);
command_match(#irc_message{trailing = []}, _Cmd) ->
    false;
command_match(#irc_message{trailing = Trailing}, 
              #command{match = {cmd, Match}, prefix = Prefix}) ->
    [Cmd|_] = binary:split(Trailing, <<" ">>),
    PrefixLen = binary:longest_common_prefix([Prefix, Cmd]),
    CmdPart = {PrefixLen, byte_size(Cmd) - PrefixLen},
    (byte_size(Prefix) == PrefixLen) andalso (Match == binary_part(Cmd, CmdPart));
command_match(#irc_message{trailing = Trailing}, 
              #command{match = {re, Match}}) ->
    re:run(Trailing, Match, [{capture, none}]) == match.


strip_command(#irc_message{trailing = Trailing}, #command{match = all}) -> Trailing;
strip_command(#irc_message{trailing = Trailing}, #command{match = {cmd, Match}, prefix = Prefix}) ->
    S = byte_size(Match) + byte_size(Prefix),
    case Trailing of
        <<_:S/binary, $ , A/binary>> -> A;
        <<_:S/binary, A/binary>> -> A
    end,
    tokens(A, <<$ >>).


%%% Generic event handler
handle({Irc, Msg}, Commands) ->
    lager:debug("got ~p~n", [Msg#irc_message.params]),
    [try_command({Irc, Msg}, C) || C <- Commands].

try_command({Irc, Msg}, Command) when Msg#irc_message.command == Command#command.event ->
    case command_match(Msg, Command) of
        true ->
            try parse_args(Msg, Command) of
                Args -> spawn(fun() -> (?MODULE):runner(Command, {Irc, Msg}, Args) end)
            catch _:_ ->
                usage({Irc, Msg}, Command)
            end;
        false -> nop
    end;
try_command(_, _) -> nop.


runner(#command{function={Mod, Fun}}, {Irc, Msg}, Args) ->
    lager:info("launching ~p:~p(~p)...", [Mod, Fun, [Msg|Args]]),
    try apply(Mod, Fun, [{Irc, Msg}|Args]) of
        {reply, Out} -> eiko_lib:reply(Irc, Msg, Out);
        {error, Reason} -> eiko_lib:reply(Irc, Msg, Reason);
        _ -> ok
    catch
        throw:{X, R} ->
            eiko_lib:reply(Irc, Msg, [atom_to_list(X), ": ", [io_lib:format("~p", [R])]]),
            lager:info("error: ~p", [erlang:get_stacktrace()]);
        _:_ ->
            lager:info("error: ~p", [erlang:get_stacktrace()])
    end.

usage({Irc, Msg}, #command{match = C, usage = U}) ->
    eiko_lib:reply(Irc, Msg, ["usage: ", io_lib:format("~p", [C]), " ", U]).

tokens(Bin, Sep) when is_list(Sep) ->
    tokens(Bin, unicode:characters_to_binary(Sep));
tokens(Bin, Sep) ->
    binary:split(Bin, Sep).


strip(Binary) ->
    strip(Binary, [], false).
%%% Don't strip within double quotes
strip(<<$", Rest/binary>>, Acc, false) ->
    strip(Rest, [$"|Acc], true);
strip(<<$", Rest/binary>>, Acc, true) ->
    strip(Rest, [$"|Acc], false);

strip(<<X, Rest/binary>>, Acc, false) 
        when X == $ ; X == $\t; X == $\r; X == $\n ->
    strip(Rest, Acc, false);
strip(<<X, Rest/binary>>, Acc, State) ->
    strip(Rest, [X|Acc], State);
%%% What to do if only an opening double quote?
strip(<<>>, Acc, false) ->
    list_to_binary(lists:reverse(Acc)).



parse_args(#irc_message{} = Msg, #command{args = Opt} = Cmd) ->
    ArgList = strip_command(Msg, Cmd),
    io:format("PreA: ~p~n", [ArgList]),
    parse_args(ArgList, Opt);
parse_args(M, Opt) ->
    A = parse_args({M, Opt, []}),
    io:format("A: ~p~n", [A]),
    lists:reverse(A).

parse_args({[<<>>|R], S, Acc}) -> parse_args({R, S, Acc}); %strip spurious whitspace
parse_args({[], {optional, _S}, Acc}) -> {[], [], [[]|Acc]};
parse_args({P, {optional, S}, Acc}) ->
    try parse_args({P, S, Acc}) catch
        _:_X -> io:format("X: ~p~n", [_X]), {P, [], [[]|Acc]}
    end;

parse_args({P, {opt, Spec}, Acc}) ->
    Args = erlopt:getopt(Spec, [binary_to_list(X) || X <- P]),
    {NP, NA} = lists:foldl(
        fun({opt, Opt}, {Np, Na}) -> {Np, [Opt|Na]};
           ({arg, Arg}, {Np, Na}) -> {[Arg|Np], Na};
           ([], {Np, Na}) -> {Np, [[]|Na]}
       end, {[],[]}, Args),
    io:format("NP: ~p~n", [NP]),
    {[list_to_binary(X) || X <- NP], [], [NA|Acc]};

parse_args({P, something, Acc}) when is_list(P) ->
    N = string:join([binary_to_list(X) || X <- P], " "),
    {[], [], [list_to_binary(N)|Acc]};
parse_args({P, something, Acc}) ->
    {[], [], [P|Acc]};

parse_args({<<$#, C/binary>>, channel, Acc}) ->
    {[], [], [<<$#, C/binary>>|Acc]};
parse_args({P, channel, Acc}) when is_list(P) ->
    {_, _, Acc1} = parse_args({hd(P), channel, Acc}),
    {tl(P), [], Acc1};
parse_args({[H|R], user, Acc}) ->
    {R, [], [H|Acc]};

parse_args({[], [], Acc}) -> Acc;
parse_args({P, [S|RS], Acc}) ->
    {NP, _, NA} = parse_args({P, S, Acc}),
    parse_args({NP, RS, NA}).


