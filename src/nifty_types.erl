%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_types).
-export([check_type/2,
	 check_type/3,
	 resolve_type/2]).

-define(CLANG_BUILTINS, ["__int128_t", "__builtin_va_list", "__uint128_t"]).
-define(CLANG_BLACKLIST, ["__builtin_va_list",
			  "__va_list_tag",
			  "__va_list_tag [1]"]).

%% @doc takes a type and a type table and returns the resolved type (according to the type table)
-spec resolve_type(nifty_clangparse:ctype(), nifty_clangparse:type_table()) -> nifty_clangparse:ctype() | undef.
resolve_type(Type, Types) ->
    case resolve_type2(Type, nifty:get_types()) of
	undef -> resolve_type2(Type, Types);
	T -> T
    end.

resolve_type2(Type, Types) ->
    case dict:is_key(Type, Types) of 
	true ->
	    {Kind, TypeDef} = dict:fetch(Type, Types),
	    case Kind of
		typedef -> resolve_type(TypeDef, Types);
		_ -> Type
	    end;
	false ->
	    undef
    end.

%% @doc Checks if <code>Type</code> is a valid type
-spec check_type(nifty_clangparse:ctype(), nifty_clangparse:type_table()) -> boolean().
check_type(Type, Types) ->
    (not lists:member(Type, ?CLANG_BLACKLIST)) andalso
	check_type(Type, Types, undef).

%% @doc Checks if <code>Type</code> is a valid type
-spec check_type(nifty_clangparse:ctype(), nifty_clangparse:type_table(), nifty_clangparse:constr_table() | undef) -> boolean().
check_type(Type, Types, Constructors) ->
    (not lists:member(Type, ?CLANG_BLACKLIST)) andalso
						 (check_type2(Type, nifty:get_types(), dict:new()) orelse 
						  check_type2(Type, Types, Constructors)).

check_type2(Type, Types, Constructors) ->
    case dict:is_key(Type, Types) of
	true ->
	    case resolve_type(Type, Types) of
		undef -> 
		    false;
		RType ->
		    case dict:fetch(RType, Types) of
			{userdef, [RType]} ->
			    false; %% loop
			{userdef, [T]} ->
			    %% constructor or dead end
			    case T of
				{struct, Name} ->
				    case Constructors of
					undef -> true;
					C -> dict:is_key({struct, Name}, C)
				    end;
				_->
				    case T=:=Type of
					true ->
					    false; %% loop
					false ->
					    check_type(T, Types) %% something else
				    end
			    end;
			{userdef, [T, "const"]} ->
			    %% discard const and check again
			    check_type(T,Types);
			{userdef, [H|T]} ->
			    string:right(H, 1) =/= ")"            %% function pointer
				andalso lists:last(T) =/= "union" %% union
				andalso lists:last(T) =/= "enum"; %% enum
			{base, _} ->
			    true
		    end
	    end;
	false ->
	    false
    end.
