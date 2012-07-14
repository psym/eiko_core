-module(eiko_core).

-behaviour(application).

-export([ start/0
        , start/2
        , stop/1
        , profile_output/0
    ]).

start() ->
    application:start(lager),
    application:start(eiko_core).

-spec start(normal | {takeover, node()} | {failover, node()}, any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    lager:start(),
    consider_profiling(),
    eiko_sup:start_link([]).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

-spec profile_output() -> ok.
profile_output() ->
    eprof:stop_profiling(),
    eprof:log("procs.profile"),
    eprof:analyze(procs),
    eprof:log("total.profile"),
    eprof:analyze(total).

-spec consider_profiling() -> profiling | not_profiling.
consider_profiling() ->
    case application:get_env(profile) of
        {ok, true} ->
            {ok, _Pid} = eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.
