-module(nifty_tags).
-export([
	insert/1,
	struct_name/1,
	struct_dereference/1,
	struct_reference/1]).

struct_name([{name, Type}]) ->
	Splited = string:tokens(Type, " "),
	case Splited of
		["struct", TypeName, _] ->
			TypeName;
		["struct", TypeName] ->
			TypeName
	end.

struct_dereference([{name, Type}]) ->
	Splited = string:tokens(Type, " "),
	case Splited of
		["struct", _, P] ->
			string:copies("*", length(P)-1);
		["struct", _] ->
			"&"
	end.

struct_reference([{name, Type}]) ->
	Splited = string:tokens(Type, " "),
	case Splited of
		["struct", _, P] ->
			string:copies("&", length(P)-1);
		["struct", _] ->
			"*"
	end.

% ETS insert tag
insert([{table, ETSString}|T]) ->
	StringKeyValue = [{atom_to_list(K), V} || {K,V} <- T],
	{ETS, _} = string:to_integer(ETSString),
	ets:insert(ETS, StringKeyValue),
	"".
