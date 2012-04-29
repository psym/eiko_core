-module(eiko_lib).

-include("irc.hrl").

%%% API
-export([ reply/3
        , parse/1
        , get_orgin/1
        , get_sender/2
    ]).

reply(Irc, Msg, Response) ->
    eiko_network:send(Irc, ["PRIVMSG", " ", get_orgin(Msg)], Response).

-spec get_orgin(#irc_message{}) -> binary().
get_orgin(#irc_message{params=[<<"#", Channel/binary>>|_]} = _Msg) ->
    <<"#", Channel/binary>>;
get_orgin(Msg) ->
    get_sender(Msg, [nick]).

get_sender(#irc_message{prefix = Prefix}, Args) ->
    get_sender(Prefix, Args);
get_sender(Prefix, Args) ->
    case re:split(Prefix, "!(.*)@") of
        [Nick, Name, Host] ->
            P = [{nick, Nick}, {name, Name}, {host, Host}];
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
    [Middle | Trailing] = binary:split(Rest, <<" :">>),
    [Command | Params] = binary:split(Middle, <<" ">>, [global]),
    #irc_message{prefix = Prefix, command = Command, params = Params, trailing = Trailing}.
