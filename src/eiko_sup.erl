-module(eiko_sup).

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
        {eiko_log, {eiko_log, start_link, []},
            Restart, Shutdown, worker, [eiko_log]},
        {eiko_cfg, {eiko_cfg, start_link, []}, 
            Restart, Shutdown, worker, [eiko_cfg]},
        {eiko_network_sup, {eiko_network_sup, start_link, [ok]},
            Restart, Shutdown, supervisor, [eiko_network_sup]}
    ],
    {ok, {SupFlags, Children}}.

