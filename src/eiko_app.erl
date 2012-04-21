-module(eiko_app).

-behaviour(application).

-export([
        start/0,
        start/2,
        stop/1
    ]).

start() ->
    application:start(eiko).

start(_StartType, _StartArgs) ->
    lager:start(),
    irc_sup:start_link([]).

stop(_State) ->
    unimplemented.
