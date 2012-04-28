-module(irc_network_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).


start_link(_) -> supervisor:start_link(?MODULE, []).

init(_) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Networks = [
        {Network, {irc_network, start_link, [Network]}, 
            Restart, Shutdown, Type, [irc_network]}
        || {Network, _} <- eiko_cfg:networks() 
    ],

    {ok, {SupFlags, Networks}}.

