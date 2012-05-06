-module(eiko_access).

-export([ has_access/2
        , is_owner/1
    ]).

-include("irc.hrl").

-spec is_owner(#irc_message{} | binary()) -> true | false.
is_owner(Prefix) ->
     [Nick, User, Host] = eiko_lib:get_sender(Prefix, [nick, user, host]),
     lists:any(
         fun([N, U, H]) -> 
            ((N == Nick) or (N == <<"*">>)) and
            ((U == User) or (U == <<"*">>)) and
            ((H == Host) or (H == <<"*">>))
         end,
         [eiko_lib:get_sender(Owner, [nick, user, host]) || Owner <- eiko_cfg:owners()]
     ).

has_access(_Prefix, all) ->
    true;
has_access(Prefix, owner) ->
    is_owner(Prefix);
has_access(_Prefix, _Type) ->
    false.
