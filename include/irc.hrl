
-record(irc_channel, {
        name :: string(),
        mode,
        users :: [term()]
    }).

-record(irc_state, {
        ref     :: pid(),
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
