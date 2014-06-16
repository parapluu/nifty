%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_clangparse).
-export([parse/1, build_type_table/2]).
-on_load(init/0).
-export_type([parser_output/0,
	      func_location/0,
	      symbol_table/0,
	      ctype/0,
	      types/0,
	      type_table/0,
	      constr_table/0]).

-type path() :: string().
-type cname() :: nonempty_string().
-type ctype() :: nonempty_string().
-type index() :: integer().
-type field() :: {'field', cname(), ctype(), index()}.
-type types() :: [ctype()].
-type constructor_type() :: 'struct'.
-type ctypedef() :: {'base', [string()]} |
		    {'typedef', ctype()} |
		    {'userdef' , [string() | {constructor_type(), nonempty_string()}]}.
-type func_location() :: dict:dict(path(), cname()).
-type symbol_table() :: dict:dict(cname(), {'return', ctype()} | {'argument', index(), ctype()}).
-type type_table() :: dict:dict(ctype(), ctypedef()).
-type constr_table() :: dict:dict({'struct' | 'typedef', cname()}, ctype() | [field()]).
-type parser_output() :: {func_location(), symbol_table(), types(), constr_table()}.

-define(BASE_TYPES, ["char", "int", "float", "double", "void"]).
-define(SPECIFIER, ["signed", "unsigned", "short", "long"]).

init() -> %% loading code from jiffy
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, _} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
		      AppPath = filename:dirname(EbinDir),
		      filename:join(AppPath, "priv");
		  Path ->
		      Path
	      end,
    erlang:load_nif(filename:join(PrivDir, "nifty_clangparse"), 0).

cparse(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Takes clang compiler arguments, and returns functions locations, symbol table, types and constructor table
-spec parse([string()]) -> parser_output() | {error, fail}.
parse(Args) ->
    case cparse(Args) of
	fail ->
	    {error, fail};
	{FL, ST, T, CT} ->
	    {dict:from_list(FL),
	     dict:from_list(ST),
	     T,
	     dict:from_list(CT)}
    end.

%% @doc building the type table from the raw parser output
-spec build_type_table(types(), constr_table()) -> type_table().
build_type_table(Types, Constr) ->
    Constr_Types = fill_constructed(Constr, dict:new()),
    Used_Types = fill_types(Types, Constr_Types),
    fill_type_table(Used_Types).

fill_constructed(Constr, Types) ->
    fill_constructed(dict:fetch_keys(Constr), Constr, Types).

fill_constructed([], _, Types) ->
    Types;
fill_constructed([H|T], Constr, Types) ->
    Updated_Types = case H of
			{typedef, Alias} ->
			    Type = dict:fetch(H, Constr),
			    case is_fptr(Type) of 
				true ->
				    dict:store(Alias, {typedef, "void *"}, Types);
				false ->
				    dict:store(Alias, {typedef, Type}, Types)
			    end;
			{struct, Name} ->
			    D = dict:store("struct "++Name, {userdef, [H]}, Types),
			    dict:store("struct "++Name++" *", {userdef, ["*", H]}, D)
		    end,
    fill_constructed(T, Constr, Updated_Types).

fill_types([], Types) ->
    Types;
fill_types([Type|T], Types) ->
    fill_types(T, build_type_entry(Types, Type)).

fill_type_table(Types) ->
    fill_type_table(Types, dict:fetch_keys(Types)).

fill_type_table(Types, []) -> Types;
fill_type_table(Types, [Type|TypeNames]) ->
    {Kind, L} = dict:fetch(Type, Types),
    case Kind of
	base -> fill_type_table(Types, TypeNames);
	typedef -> fill_type_table(Types, TypeNames);
	userdef ->
	    [H|T] = L,
	    case H of
		{_,_} ->
		    fill_type_table(Types, TypeNames);
		_ ->
		    case (H=:="*") orelse string:str(H, "[")>0 of
			true ->
			    [N|_] = T,
			    case N of
				{_,_} ->
				    fill_type_table(Types, TypeNames);
				_ ->
				    [P|Token] = lists:reverse(string:tokens(Type, " ")),
				    NewP = string:substr(P, 1, length(P) - length(H)),
				    NType = string:strip(string:join(lists:reverse(Token)++[NewP], " ")),
				    case dict:is_key(NType, Types) of 
					true -> fill_type_table(Types, TypeNames);
					false -> fill_type_table(dict:store(NType, {Kind, T}, Types), [NType|TypeNames])
				    end
			    end;
			false -> fill_type_table(Types, TypeNames)
		    end
	    end
    end.

count_in_list(L, E) ->
    count_in_list(L,E,0).

count_in_list([], _, Acc) -> Acc;
count_in_list([H|T], E, Acc) ->
    case H =:= E of
	true -> count_in_list(T, E, Acc+1);
	false -> count_in_list(T, E, Acc)
    end.


simplify_specifiers(Specifiers) ->
    LSpec = case count_in_list(Specifiers, "long") of
		0 ->
		    case lists:member("short", Specifiers) of
			true -> ["short"];
			false -> ["none"]
		    end;
		1 -> ["long"];
		_ -> ["longlong"]
	    end,
    case count_in_list(Specifiers, "unsigned") of
	0 -> ["signed"|LSpec];
	_ -> ["unsigned"|LSpec]
    end.

parse_type(Token) ->
    parse_type(Token, [], none).

parse_type([], TypeDef, none) -> parse_type(["int"], TypeDef, none);
parse_type([], TypeDef, Kind) -> {Kind, TypeDef};
parse_type([E|T], TypeDef, Kind) ->
    case E of
	%% special cases
	"struct" ->
	    [StructName|TT] = T,
	    parse_type(TT, [{struct, StructName}|TypeDef], userdef);
	_ ->
	    %% simple type
	    case lists:member(E, ?BASE_TYPES) of
		true -> parse_type(T, [E|simplify_specifiers(TypeDef)], base);
		false -> 
		    case lists:member(E, ?SPECIFIER) of
			true -> parse_type(T, [E|TypeDef], none);
			false -> 
			    case ((E=:="*") or lists:member($[, E)) of
				true ->
				    case Kind of
					none -> parse_type(["int"|[E|T]], TypeDef, base);
					_ -> parse_type(T, [E|TypeDef], Kind)
				    end;
				false ->
				    %% user defined type
				    parse_type(T, [E|TypeDef], userdef)
			    end
		    end
	    end
    end.

type_extend(Type) ->
    type_extend(Type, []).

type_extend([], Acc) -> Acc;
type_extend([H|T], Acc) ->
    case [H] of
	"*" -> type_extend(T, Acc++" * ");
	"[" -> type_extend(T, Acc++" [");
	C -> type_extend(T, Acc++C)
    end.

norm_type(Type) ->
    case string:str(Type, "[]") of
	0 -> Type;
	P -> string:substr(Type, 1, P-1)++"*"
    end.

is_fptr(Type) ->
    string:str(Type, "(")=/=0.


build_type_entry(TypeTable, Type) ->
    NType = norm_type(Type),
    case dict:is_key(NType, TypeTable) of
	true -> 
	    TypeTable;
	false->
	    case is_fptr(NType) of
		true ->
		    TDef_Table = dict:store(NType, {typedef, "void *"}, TypeTable),
		    build_type_entry(TDef_Table, "void *");
		false ->
		    Def = parse_type(string:tokens(type_extend(NType), " ")),
		    dict:store(NType, Def, TypeTable)
	    end
    end.

