
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
        ref     :: pid(),
        network     :: atom(),
        nick,
        channels    :: #irc_channel{}
    }).


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
        args,
        usage,
        prefix = <<"">>
    }).

-record(eiko_plugin, {
        event       :: pid(),
        commands
    }).

