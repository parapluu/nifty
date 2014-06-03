%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_filters).
-export([raw_include/1,
	 raw_path/1,
	 absname/1,
	 getNth/2,
	 resolved/2,
	 reversed/1,
	 fetch/2,
 	 fetch_keys/1,
	 has_key/2,
	 is_argument/1,
	 is_return/1,
	 is_field/1,
	 is_input/1,
	 is_output/1,
	 is_array/1,

	 array_size/1,
	 array_type/1,

	 norm_type/1,
	 dereference_type/1,
	 discard_const/1,
	 discard_restrict/1,
	 loopcounter/2]).

-spec norm_type(string()) -> string().
norm_type(Type) ->
    case string:str(Type, "[") of
	0 -> Type;
	S -> case string:str(Type, "]") of
		 0 -> Type; % error;
		 E -> norm_type(string:substr(Type, 1, S-1) ++ "*" ++ string:substr(Type, E+1))
	     end
    end.

-spec dereference_type(string()) -> string().
dereference_type(Type) ->
    NType = norm_type(Type),
    case string:str(NType, "*") of
	0 -> NType;
	S -> string:strip(string:substr(NType, 1, S-1) ++ string:substr(NType, S+1))
    end.

-spec discard_const(string()) -> string().
discard_const(Type) ->
    string:join([Tok || Tok <- string:tokens(Type, " "), Tok =/= "const"], " ").

-spec loopcounter(string(), string()) -> string().
loopcounter(Type, Name) ->
    NType = "#"++norm_type(Type)++"#",
    Type_ = string:join(string:tokens(NType, " "), "_"),
    Enclosed = string:join(string:tokens(Type_, "*"), "P"),
    "index_"++Name++"_"++string:substr(Enclosed, 2, length(Enclosed)-2).

%%% special
-spec raw_include(string()) -> string().
raw_include(Path) ->
    lists:last(filename:split(Path)).

-spec raw_path(string()) -> string().
raw_path(Path) ->
    filename:dirname(Path).

-spec absname(string()) -> string().
absname(Path) ->
    filename:absname(Path).

%%% general
-spec getNth(list() | tuple(), integer()) -> term().
getNth(I, N) when is_list(I) ->
    lists:nth(N, I);
getNth(I, N) when is_tuple(I) ->
    lists:nth(N, tuple_to_list(I)).

-spec reversed([X]) -> [X].
reversed(L) ->
    lists:reverse(L).


-spec discard_restrict(string()) -> string().
discard_restrict(Type) ->
    case string:str(Type, "restrict") of
    	0 ->
    	    Type;
    	P ->
    	    string:strip(string:substr(Type, 1,P-1) ++ string:substr(Type, P+length("restrict")))
    end.

-spec resolved(string(), dict:dict()) -> string().
resolved(Type, Types) ->
    nifty_types:resolve_type(Type, Types).

%%% dict
-spec fetch(dict:dict(), string()) -> term().
fetch(Dict, Key) ->
    dict:fetch(Key, Dict).

-spec fetch_keys(dict:dict()) -> list().
fetch_keys(Dict) ->
    dict:fetch_keys(Dict).

-spec has_key(dict:dict(), string()) -> boolean().
has_key(Dict, Key) ->
    dict:is_key(Key, Dict).

%%% symbol table entries
-spec is_argument(string()) -> boolean().
is_argument(Arg) ->
    getNth(Arg, 1)=:=argument.

-spec is_return(string()) -> boolean().
is_return(Arg) ->
    getNth(Arg, 1)=:=return.

-spec is_field(string()) -> boolean().
is_field(Arg) ->
    getNth(Arg, 1)=:=field.

-spec is_input(string()) -> boolean().
is_input(Arg) ->
    (getNth(Arg, 4)=:=input) orelse (getNth(Arg, 4)=:=inoutput).

-spec is_output(string()) -> boolean().
is_output(Arg) ->
    (getNth(Arg, 4)=:=output) orelse (getNth(Arg, 4)=:=inoutput).

-spec is_array(string()) -> boolean().
is_array(Arg) -> 
    (is_list(Arg) andalso (string:str(Arg, "[")=:=1 andalso length(Arg)>2)).


-spec array_type(string()) -> string().
array_type(Type) ->
    Token = string:tokens(Type, " "),
    array_type_build(Token, []).

array_type_build([H|T], Acc) ->
    case string:str(H, "[")=:=1 andalso lists:last(H)=:=$] of
	true ->
	    string:strip(Acc);
	false ->
	    array_type_build(T, Acc++" "++H)
    end.
       

-spec array_size({base | userdef , [string()]}) -> integer().
array_size({_, Typedef}) ->
    array_size(Typedef, 1).

array_size([H|T], Acc) ->
    case string:str(H, "[")=:=1 andalso lists:last(H)=:=$] of
	true ->
	    {Size,[]} = string:to_integer(string:substr(H, 2, length(H)-2)),
	    array_size(T, Acc*Size);
	false ->
	    Acc
    end.
