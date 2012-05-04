-module(eiko_lib).

-include("irc.hrl").

%%% API
-export([ reply/3
        , parse/1
        , get_origin/1
        , get_sender/2
    ]).

reply(Irc, Msg, Response) ->
    eiko_network:send(Irc, ["PRIVMSG", " ", get_origin(Msg)], Response).

-spec get_origin(#irc_message{}) -> binary().
get_origin(#irc_message{params=[<<"#", Channel/binary>>|_]} = _Msg) ->
    <<"#", Channel/binary>>;
get_origin(Msg) ->
    get_sender(Msg, [nick]).

%%% a user irc prefix has the form "nick!user@host"
%%% Args is a list containing any combination of nick, user, host
%%% a server prefix will only ever have a host
get_sender(#irc_message{prefix = Prefix}, Args) ->
    get_sender(Prefix, Args);
get_sender(Prefix, Args) ->
    case re:split(Prefix, "!(.*)@") of
        [Nick, Name, Host] ->
            P = [{nick, Nick}, {user, Name}, {host, Host}];
        [Host] ->
            P = [{host, Host}]
    end,
    lists:reverse(
        lists:foldl(fun(A, Out) -> [kvc:value(A, P, []) | Out] end, [], Args)
    ).



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
    [Middle, Trailing] = case binary:split(Rest, <<" :">>) of
        [H, T] -> [H, T];
        [H]    -> [H, undefined]
    end,
    [Command | Params] = binary:split(Middle, <<" ">>, [global]),
    #irc_message{prefix = Prefix, command = Command, params = Params, trailing = Trailing}.
