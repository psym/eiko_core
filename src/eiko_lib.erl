-module(eiko_lib).

-include("irc.hrl").

%%% API
-export([
        parse/1,
        send/3,

        join/2,
        nick/2, user/3
    ]).

nick(Conn, Nick) -> 
    send(Conn, "NICK", Nick).

user(Conn, User, RealName) ->
    send(Conn, ["USER ", User, " 0 * "], [RealName]).

join(Conn, Channel) ->
    send(Conn, "JOIN", Channel).

send(_, _, []) -> ok;
send(Conn, Pre, Data) ->
    D1 = iolist_to_binary(Data),
    Lim = 510 - iolist_size(Pre) -2,
    Size = iolist_size(D1),
    case min(Lim, Size) of
        Lim ->
            ToSend = binary:part(D1, 0, Lim),
            Rest = binary:part(D1, Lim, Size-Lim);
        Size ->
            ToSend = D1,
            Rest = []
    end,
    %XXX: fix this
    gen_fsm:send_all_state_event(Conn, {out, [Pre | [<<$:, ToSend/binary>>]]}),
    send(Conn, Pre, Rest).

%% message    =  [ ":" prefix SPACE ] command [ params ] crlf
%% prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
%% command    =  1*letter / 3digit
%% params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
%%            =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]

%% nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
%%                 ; any octet except NUL, CR, LF, " " and ":"
%% middle     =  nospcrlfcl *( ":" / nospcrlfcl )
%% trailing   =  *( ":" / " " / nospcrlfcl )
%% SPACE      =  %x20        ; space character
%% crlf       =  %x0D %x0A   ; "carriage return" "linefeed"
-spec parse(binary() | list()) -> #irc_message{}.
parse(Line) when is_list(Line) ->
    parse(unicode:characters_to_binary(Line));
parse(Line) when is_binary(Line) ->
    [Prefix, Rest] = case Line of
        <<":", R/binary>> -> binary:split(R, <<" ">>);
        _ -> [undefined, Line]
    end,
    [Middle | Trailing] = binary:split(Rest, <<" :">>),
    [Command | Params] = binary:split(Middle, <<" ">>, [global]),
    #irc_message{prefix = Prefix, command = Command, params = Params, trailing = Trailing}.
