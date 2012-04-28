-module(irc_sup).

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

    Children = [
        {irc_log, {irc_log, start_link, []},
            Restart, Shutdown, worker, [irc_log]},
        {eiko_cfg, {eiko_cfg, start_link, []}, 
            Restart, Shutdown, worker, [eiko_cfg]},
        {irc_network_sup, {irc_network_sup, start_link, [ok]},
            Restart, Shutdown, supervisor, [irc_network_sup]}
    ],
    {ok, {SupFlags, Children}}.

