-module(nifty_filters).
-export([
	lookup/2,
	norm_type/1,
	dereference_type/1,
	loopcounter/2]).

norm_type(Type) ->
	case string:str(Type, "[") of
		0 -> Type;
		S -> case string:str(Type, "]") of
			0 -> Type; % error;
			E -> norm_type(string:substr(Type, 1, S-1) ++ "*" ++ string:substr(Type, E+1))
		end
	end.


dereference_type(Type) ->
	NType = norm_type(Type),
	case string:str(NType, "*") of
		0 -> NType;
		S -> string:strip(string:substr(NType, 1, S-1) ++ string:substr(NType, S+1))
	end.

loopcounter(Type, Name) ->
	NType = "#"++norm_type(Type)++"#",
	Type_ = string:join(string:tokens(NType, " "), "_"),
	Enclosed = string:join(string:tokens(Type_, "*"), "P"),
	"index_"++Name++"_"++string:substr(Enclosed, 2, length(Enclosed)-2).

% ETS lookup Element
lookup(Tab, Key) ->
 	[{_, Retval}] = ets:lookup(Tab, binary_to_list(Key)),
 	Retval.

% dict lookup
fetch(Dict, Key) ->
	dict:fetch(binary_to_list(Key), Dict).

is_key(Dict, Key) ->
	dict:is_key(binary_to_list(Key), Dict).
