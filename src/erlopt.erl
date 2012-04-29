%%%     Copyright 2009 Gerald Gutierrez
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
-module(erlopt).
-include_lib("eunit/include/eunit.hrl").

-author("gerald.gutierrez@gmail.com").

-export([getopt/2]).

% -------------------------------------------------------------------------
% API

getopt(Spec, Args) -> idle(Spec, elements(Args), []).

% -------------------------------------------------------------------------
% Internal functions

elements(Args) -> lists:foldr(fun(E, A) -> E ++ [ee] ++ A end, [el], Args).

option_type(Spec, short, Opt)  -> option_type(Spec, 1, Opt);
option_type(Spec, long, Opt)   -> option_type(Spec, 2, Opt);
option_type(Spec, Column, Opt) -> 
    case lists:keyfind(Opt, Column, Spec) of
        false        -> throw({erlopt, bad_option, Opt});
        {_, _, Type} -> Type
    end.

short_option(Spec, [H|T], Acc) ->
    case option_type(Spec, short, [H]) of
        no    -> osn1(Spec, T, [{opt, {[H], []}}|Acc]);
        maybe -> osm1(Spec, T, Acc, {H, []});
        yes   -> osy1(Spec, T, Acc, {H, []})
    end.

stop(Acc)                         -> lists:reverse(Acc).

idle(_, [el|_], Acc)              -> stop(Acc);
idle(Spec, [$-|T], Acc)           -> dsh1(Spec, T, Acc);
idle(Spec, [H|T], Acc)            -> arg1(Spec, T, Acc, [H]).

arg1(Spec, [ee|T], Acc, S)        -> idle(Spec, T, [{arg, lists:reverse(S)}|Acc]);
arg1(Spec, [H|T], Acc, S)         -> arg1(Spec, T, Acc, [H|S]).

dsh1(Spec, [$-|T], Acc)           -> dsh2(Spec, T, Acc);
dsh1(Spec, [ee|T], Acc)           -> idle(Spec, T, [{arg, "-"}|Acc]);
dsh1(Spec, Elems, Acc)            -> short_option(Spec, Elems, Acc).

dsh2(_, [ee|T], Acc)              -> arg2(T, Acc, []);
dsh2(Spec, [H|T], Acc)            -> oln1(Spec, T, Acc, {[H], []}).

arg2([el|_], Acc, _)              -> stop(Acc);
arg2([ee|T], Acc, S)              -> arg2(T, [{arg, lists:reverse(S)}|Acc], []);
arg2([H|T], Acc, S)               -> arg2(T, Acc, [H|S]).

osn1(Spec, [ee|T], Acc)           -> idle(Spec, T, Acc);
osn1(Spec, Elems, Acc)            -> short_option(Spec, Elems, Acc).

osm1(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {[Opt], lists:reverse(S)}}|Acc]);
osm1(Spec, [H|T], Acc, {Opt, S})  -> osm1(Spec, T, Acc, {Opt, [H|S]}).

osy1(Spec, [ee|T], Acc, {Opt, S}) -> osy2(Spec, T, Acc, {Opt, S});
osy1(Spec, [H|T], Acc, {Opt, S})  -> osy3(Spec, T, Acc, {Opt, [H|S]}).

osy2(_, [el|_], _, {Opt, _})      -> throw({erlopt, option_requires_argument, [Opt]});
osy2(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {[Opt], lists:reverse(S)}}|Acc]);
osy2(Spec, [H|T], Acc, {Opt, S})  -> osy2(Spec, T, Acc, {Opt, [H|S]}).

osy3(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {[Opt], lists:reverse(S)}}|Acc]);
osy3(Spec, [H|T], Acc, {Opt, S})  -> osy2(Spec, T, Acc, {Opt, [H|S]}).

oln1(Spec, [ee|T], Acc, {Opt, S}) ->
    Opt2 = lists:reverse(Opt),
    case option_type(Spec, long, Opt2) of
        no    -> idle(Spec, T, [{opt, {Opt2, lists:reverse(S)}}|Acc]);
        maybe -> idle(Spec, T, [{opt, {Opt2, lists:reverse(S)}}|Acc]);
        yes   -> oln2(Spec, T, Acc, {Opt2, S})
    end;

oln1(Spec, [$=|T], Acc, {Opt, S}) ->
    Opt2 = lists:reverse(Opt),
    case option_type(Spec, long, Opt2) of
        no    -> throw({erlopt, option_requires_no_argument, Opt2});
        maybe -> oln3(Spec, T, Acc, {Opt2, S});
        yes   -> oln3(Spec, T, Acc, {Opt2, S})
    end;

oln1(Spec, [H|T], Acc, {Opt, S})  -> oln1(Spec, T, Acc, {[H|Opt], S}).

oln2(_, [el|_], _, {Opt, _})      -> throw({erlopt, option_requires_argument, Opt});
oln2(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {Opt, lists:reverse(S)}}|Acc]);
oln2(Spec, [H|T], Acc, {Opt, S})  -> oln2(Spec, T, Acc, {Opt, [H|S]}).

oln3(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {Opt, lists:reverse(S)}}|Acc]);
oln3(Spec, [H|T], Acc, {Opt, S})  -> oln2(Spec, T, Acc, {Opt, [H|S]}).

% -------------------------------------------------------------------------
% EUnit

eunit_test() -> 
    ok.

short_options_test() ->
    Spec = [
        {"a", "alfa"    , no}, 
        {"b", "bravo"   , no}, 
        {"c", "charlie" , no}, 
        {"d", "delta"   , maybe}, 
        {"e", "echo"    , maybe}, 
        {"f", "foxtrot" , yes}, 
        {"g", "golf"    , yes}
    ],

    Args = ["-a", "-bc", 
        "-cd", "-cdxxx",
        "-e", "-exxx", 
        "-cf", "yyy",
        "-f", "yyy",
        "-cgzzz",
        "-gzzz"],

    [   {opt,{"a",[]}},
        {opt,{"b",[]}},
        {opt,{"c",[]}},
        {opt,{"c",[]}},
        {opt,{"d",[]}},
        {opt,{"c",[]}},
        {opt,{"d","xxx"}},
        {opt,{"e",[]}},
        {opt,{"e","xxx"}},
        {opt,{"c",[]}},
        {opt,{"f","yyy"}},
        {opt,{"f","yyy"}},
        {opt,{"c",[]}},
        {opt,{"g","zzz"}},
        {opt,{"g","zzz"}}] = getopt(Spec, Args).

long_options_test() ->
    Spec = [
        {"a", "alfa"    , no}, 
        {"b", "bravo"   , no}, 
        {"c", "charlie" , no}, 
        {"d", "delta"   , maybe}, 
        {"e", "echo"    , maybe}, 
        {"f", "foxtrot" , yes}, 
        {"g", "golf"    , yes}
    ],

    Args = [
        "--alfa", 
        "--bravo",
        "--charlie",
        "--delta",
        "--delta=xxx",
        "--echo",
        "--echo=",
        "--echo=yyy",
        "--foxtrot=",
        "--foxtrot=zzz"],

    [   {opt,{"alfa",[]}},
        {opt,{"bravo",[]}},
        {opt,{"charlie",[]}},
        {opt,{"delta",[]}},
        {opt,{"delta","xxx"}},
        {opt,{"echo",[]}},
        {opt,{"echo",[]}},
        {opt,{"echo","yyy"}},
        {opt,{"foxtrot",[]}},
        {opt,{"foxtrot","zzz"}}] = getopt(Spec, Args).
   
mixed_options_arguments_test() ->
    Spec = [
        {"a", "alfa"    , no}, 
        {"b", "bravo"   , no}, 
        {"c", "charlie" , no}, 
        {"d", "delta"   , maybe}, 
        {"e", "echo"    , maybe}, 
        {"f", "foxtrot" , yes}, 
        {"g", "golf"    , yes}
    ],

    Args = [
        "-abcdxxx",
        "argument 1",
        "-e", "argument 2",
        "-eyyy",
        "-",
        "argument 3",
        "-f--",
        "-g", "--",
        "-a",
        "--",
        "-argument 4",
        "--argument 5",
        "argument 6"],

    [{opt,{"a",[]}},
        {opt,{"b",[]}},
        {opt,{"c",[]}},
        {opt,{"d","xxx"}},
        {arg,"argument 1"},
        {opt,{"e",[]}},
        {arg,"argument 2"},
        {opt,{"e","yyy"}},
        {arg,"-"},
        {arg,"argument 3"},
        {opt,{"f","--"}},
        {opt,{"g","--"}},
        {opt,{"a",[]}},
        {arg,"-argument 4"},
        {arg,"--argument 5"},
        {arg,"argument 6"}] = getopt(Spec, Args).

