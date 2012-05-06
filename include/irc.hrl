
-record(irc_user, {
        nick,
        mode,
        global_mode
    }).

-record(irc_channel, {
        name :: string(),
        mode,
        users :: dict()
    }).

-record(irc_state, {
        ref         :: pid(),
        network     :: atom(),
        event       :: pid(),
        nick        :: string(),
        channels
    }).
-define(IRCNET(Irc), Irc#irc_state.network).


-record(irc_message, {
        prefix   :: undefined | binary(),     % servername | nick@host | nick!user@host
        command  :: undefined | binary(),
        params   :: undefined | [binary()],
        trailing :: undefined | binary()
    }).

-record(command, {
        event,
        match,
        function,
        args = [],
        usage,
        prefix = undefined
    }).

-record(eiko_plugin, {
        name        :: atom(),
        event       :: pid(),
        commands
    }).

