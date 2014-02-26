-module(nifty_filters).
-export([
	 raw_include/1,
	 raw_path/1,
	 absname/1,
	 getNth/2,
	 resolved/2,
	 reversed/1,
	 lookup/2,
	 fetch/2,
	 fetchl/2,
	 fetch_keys/1,
	 has_key/2,
	 new_dict/1,
	 append/2,
	 with_key/2,
	 is_argument/1,
	 is_return/1,
	 is_field/1,
	 is_input/1,
	 is_output/1,
	 is_array/1,

	 norm_type/1,
% 	 array_name/1,
% 	 array_length/1,
% 	 array_get_base_type/1,

	 dereference_type/1,
	 discard_const/1,
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

discard_const(Type) ->
    string:join(lists:filter(fun(Value)->not (Value=:="const") end, string:tokens(Type, " ")), " ").

loopcounter(Type, Name) ->
    NType = "#"++norm_type(Type)++"#",
    Type_ = string:join(string:tokens(NType, " "), "_"),
    Enclosed = string:join(string:tokens(Type_, "*"), "P"),
    "index_"++Name++"_"++string:substr(Enclosed, 2, length(Enclosed)-2).

%%% special
raw_include(Path) ->
    lists:last(filename:split(Path)).

raw_path(Path) ->
    filename:dirname(Path).

absname(Path) ->
    filename:absname(Path).

%%% general
getNth(I, N) ->
    case is_list(I) of
	true ->  lists:nth(N, I);
	false -> lists:nth(N,tuple_to_list(I))
    end.

reversed(L) ->
    lists:reverse(L).

resolved(Type, Types) ->
    case dict:fetch(Type, Types) of
	[{typedef, RefType}] -> resolved(RefType, Types);
	_ -> Type
    end.

%%% ETS lookup Element
lookup(Tab, Key) ->
    [{_, Retval}] = ets:lookup(Tab, binary_to_list(Key)),
    Retval.

%%% dict
fetch(Dict, Key) ->
    V = dict:fetch(Key, Dict),
    case length(V) of
	1 -> [R] = V,R;
	_ -> V
    end.

fetchl(Dict, Key) ->
    dict:fetch(Key, Dict).

fetch_keys(Dict) ->
    dict:fetch_keys(Dict).

has_key(Dict, Key) ->
    dict:is_key(Key, Dict).

new_dict(_) ->
    dict:new().

append(Dict, Value) ->
    {Dict, Value}.

with_key({Dict, Value}, Key) ->
    dict:append(Key, Value, Dict).

%%% symbol table entries
is_argument(Arg) ->
    getNth(Arg, 1)=:=argument.

is_return(Arg) ->
    getNth(Arg, 1)=:=return.

is_field(Arg) ->
    getNth(Arg, 1)=:=field.

is_input(Arg) ->
    (getNth(Arg, 4)=:=input) orelse (getNth(Arg, 4)=:=inoutput).

is_output(Arg) ->
    (getNth(Arg, 4)=:=output) orelse (getNth(Arg, 4)=:=inoutput).

is_array(Arg) -> 
    (is_list(Arg) andalso (string:str(Arg, "[")=:=1 andalso length(Arg)>2)).

