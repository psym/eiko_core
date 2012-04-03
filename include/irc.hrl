-record(irc_message, {
        prefix   :: undefined | binary(),     % servername | nick@host | nick!user@host
        command  :: undefined | binary(),
        params   :: undefined | [binary()],
        trailing :: undefined | binary()
    }).

