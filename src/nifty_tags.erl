%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_tags).
-export([struct_name/1,
	 debug/1,
	 struct_dereference/1,
	 struct_reference/1]).

-spec struct_name([{name, string()}]) -> nonempty_string().
struct_name([{name, Type}]) ->
    Splited = string:tokens(Type, " "),
    case Splited of
	["struct", TypeName, _] ->
	    TypeName;
	["struct", TypeName] ->
	    TypeName
    end.

-spec struct_dereference([{name, string()}]) -> nonempty_string().
struct_dereference([{name, Type}]) ->
    Splited = string:tokens(Type, " "),
    case Splited of
	["struct", _, P] ->
	    string:copies("*", length(P)-1);
	["struct", _] ->
	    "&"
    end.

-spec struct_reference([{name, string()}]) -> nonempty_string().
struct_reference([{name, Type}]) ->
    Splited = string:tokens(Type, " "),
    case Splited of
	["struct", _, P] ->
	    string:copies("&", length(P)-1);
	["struct", _] ->
	    "*"
    end.

-spec debug(proplists:proplist()) -> string().
debug(DBM) ->
    io:format("DEBUG: ~n"),
    print_dbg(DBM),
    "".
print_dbg([]) -> ok;
print_dbg([{Key, Value}|T]) ->
    io:format("~p -> ~p~n", [Key, Value]),
    print_dbg(T).
