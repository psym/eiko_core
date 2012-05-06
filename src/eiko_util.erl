-module(eiko_util).

-export([ normalize/2
    ]).


normalize(K, atom) when is_atom(K) ->
    K;
normalize(K, atom) when is_list(K) ->
    try list_to_existing_atom(K)
    catch error:badarg ->
        K
    end;
normalize(K, atom) when is_binary(K) ->
    try binary_to_existing_atom(K, utf8)
    catch error:badarg ->
        K
    end;

normalize(K, string) when is_list(K) ->
    K;
normalize(K, string) when is_binary(K) ->
    binary_to_list(K);
normalize(K, string) when is_atom(K) ->
    atom_to_list(K);

normalize(K, binary) when is_binary(K) ->
    K;
normalize(K, binary) when is_atom(K) ->
    atom_to_binary(K, utf8);
normalize(K, binary) when is_list(K) ->
    iolist_to_binary(K);
normalize(K, binary) when is_pid(K) ->
    normalize(io_lib:format("~p", [K]), binary).

