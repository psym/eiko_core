-module(irc_util).

-export([
        to_binary/1
    ]).


to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
to_binary(X) when is_list(X) ->
    list_to_binary(X).
