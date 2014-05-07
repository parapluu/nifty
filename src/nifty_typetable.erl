-module(nifty_typetable).
-export([build/1,
	 check_types/2,
	 check_type/2,
	 check_symbols/2,
	 resolve_type/2]).

-define(BASE_TYPES, ["char", "int", "float", "double", "void"]).
-define(SPECIFIER, ["signed", "unsigned", "short", "long"]).
-define(CLANG_BUILTINS, ["__int128_t", "__builtin_va_list", "__uint128_t"]).
-define(CLANG_BLACKLIST, ["__builtin_va_list"]).

-type ctype() :: string().
-type field() :: {field, string(), ctype(), integer()}.
-type ctypedef() :: {'base', [string()]} | {'struct', [field()]} | {'typedef', string()}.
-type type_table() :: dict:dict(ctype(), ctypedef()).
-type argument_index() :: integer().
-type symbol_table() :: dict:dict(string(), [{'return', ctype()} | {'argument', argument_index(), ctype()}]).

%% @doc takes a type and a type table and returns the resolved type (according to the type table)
-spec resolve_type(ctype(), type_table()) -> ctype() | undef.
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

%% @doc makes the symbol table consistent with the checked function types
-spec check_symbols(nifty_clangparse:defs(), symbol_table()) -> {symbol_table(), [nonempty_string()]}.
check_symbols({Functions, _, _}, Symbols) ->
    AllSymbols = dict:fetch_keys(Symbols),
    check_symbols(AllSymbols, Functions, Symbols, dict:new(), []).

check_symbols([], _, _, Acc, L) ->
    {Acc, L};
check_symbols([H|T], Ref, Old, New, L) ->
    case dict:is_key(H, Ref) of
	true ->
	    check_symbols(T, Ref, Old, dict:store(H, dict:fetch(H, Old), New), L);
	false ->
	    io:format("Warning: Unable to translate function ~p() !!!~n", [H]),
	    check_symbols(T, Ref, Old, New, [H|L])
    end.

%% @doc removes all non-resolvable types from the type table and
%% structs or functions that depend on them and returns the filtered
%% type information
-spec check_types(nifty_clangparse:defs(), type_table()) -> {nifty_clangparse:defs(), type_table()}.
check_types(Defs, Types) ->
    {NDefs, NTypes} = check_types_once(Defs, Types),
    case length(dict:fetch_keys(Types)) =:= length(dict:fetch_keys(NTypes)) of
	true ->
	    {NDefs, NTypes};
	false ->
	    check_types(NDefs, NTypes)
    end.

check_types_once({Functions, Typedefs, Structs}, Types) ->
    NTypes = check_types_types(Types),
    {NStructs, NNTypes} = check_types_structs(Structs, NTypes),
    NFunc = check_types_functions(Functions, NNTypes),
    {{NFunc, Typedefs, NStructs}, NNTypes}.

-spec check_type(ctype(), type_table()) -> boolean().
check_type(Type, Types) ->
     check_type2(Type, nifty:get_types()) orelse check_type2(Type, Types).

check_type2(Type, Types) ->
    case dict:is_key(Type, Types) of
	true ->
	    RType = resolve_type(Type, Types),
	    case dict:fetch(RType, Types) of
		{userdef, [RType]} ->
		    false; %% loop
		{userdef, [T]} ->
		    %% constructor or dead end
		    case Type =:= T of
			true ->
			    false; %% loop - typedef with same name as constructor/no constructor available
			false ->
			    check_type(T, Types)
		    end;
		{userdef, [T, "const"]} ->
		    %% discard const and check again
		    check_type(T,Types);
		{userdef, [H|T]} ->
		    string:right(H, 1) =/= ")" andalso lists:last(T) =/= "union";  %% function pointer or union
		{struct, Fields} ->
		    check_fields(Fields, Types);
		{base, _} ->
		    true
	    end;
	false ->
	    false
    end.

check_fields([], _) -> 
    true;
check_fields([{field, _, Type, _}|T], Types) ->
    check_type(Type, Types) andalso check_fields(T, Types).

check_types_functions(Functions, Types) ->
    Names = dict:fetch_keys(Functions),
    check_types_functions(Names, Functions, dict:new(), Types).

check_types_functions([], _, NFunc, _) ->
    NFunc;
check_types_functions([Func|Tail],OldFunc,NewFunc,Types) ->
    {RetType, ArgList} = dict:fetch(Func, OldFunc),
    case check_type(RetType, Types) andalso check_types_list(ArgList, Types) of
	true ->
	    check_types_functions(Tail, 
				  OldFunc, 
				  dict:store(Func,
					     dict:fetch(Func, OldFunc),
					     NewFunc),
				  Types);
	false ->
	    check_types_functions(Tail, OldFunc, NewFunc, Types)
    end.
    
check_types_structs(Structs, Types) ->
    Names = dict:fetch_keys(Structs),
    check_types_structs(Names, Structs, dict:new(), Types).

check_types_structs([], _, NewStructs, Types) ->
    {NewStructs,Types};
check_types_structs([Struct|Tail], OldStructs, NewStructs, Types) ->
    case check_types_fields(dict:fetch(Struct, OldStructs), Types) of
	true ->
	    check_types_structs(Tail, 
				OldStructs, 
				dict:store(Struct, 
					   dict:fetch(Struct, OldStructs), 
					   NewStructs),
				Types);
	false ->
	    check_types_structs(Tail,
				OldStructs,
				NewStructs,
				dict:erase(Struct, Types))
    end.
    
check_types_fields([], _) ->
    false;
check_types_fields(Fields, Types) ->
    check_types_list(Fields, Types).

check_types_list([], _) ->
    true;
check_types_list([{_, Type}|Tail], Types) ->
    check_type(Type, Types) andalso check_types_list(Tail, Types).
    
check_types_types(Types) ->
    Names = dict:fetch_keys(Types),
    check_types_types(Names, Types, dict:new()).

check_types_types([], _, NewTypes) ->
    NewTypes;
check_types_types([Type|Tail], OldTypes, NewTypes) ->
    case check_type(Type, OldTypes) of
	true ->
	    check_types_types(Tail, 
			      OldTypes, 
			      dict:store(Type, 
					 dict:fetch(Type, OldTypes), 
					 NewTypes));
	false ->
	    check_types_types(Tail, OldTypes, NewTypes)
    end.

%% @doc builds a typetable and symbol table out of type information
-spec build(nifty_clangparse:defs()) -> {type_table(), symbol_table()}.
build({Functions, Typedefs, Structs} = Dicts) ->
    Empty_Tables = {dict:new(), dict:new()}, % { Types, Symbols }
    Tables_With_Functions = build_entries(Empty_Tables,
					  fun build_function_entries/4,
					  Functions,
					  dict:fetch_keys(Functions),
					  Dicts),
    Tables_With_TypeDefs = build_entries(Tables_With_Functions,
					 fun build_typedef_entries/4,
					 Typedefs,
					 dict:fetch_keys(Typedefs),
					 Dicts),
    {Types, Symbols} = build_entries(Tables_With_TypeDefs,
				     fun build_struct_entries/4,
				     Structs,
				     dict:fetch_keys(Structs),
				     Dicts),
    {fill_type_table(Types), Symbols}.

fill_type_table(Types) ->
    fill_type_table(Types, dict:fetch_keys(Types)).

fill_type_table(Types, []) -> Types;
fill_type_table(Types, [Type|TypeNames]) ->
    {Kind, L} = dict:fetch(Type, Types),
    case Kind of
	base -> fill_type_table(Types, TypeNames);
	struct -> fill_type_table(Types, TypeNames);
	typedef -> fill_type_table(Types, TypeNames);
	_ ->
	    [H|T] = L,
	    case (H=:="*") orelse string:str(H, "[")>0 of
		true ->
		    [P|Token] = lists:reverse(string:tokens(Type, " ")),
		    NewP = string:substr(P, 1, length(P) - length(H)),
		    NType = string:strip(string:join(lists:reverse(Token)++[NewP], " ")),
		    case dict:is_key(NType, Types) of 
			true -> fill_type_table(Types, TypeNames);
			false -> fill_type_table(dict:store(NType, {Kind, T}, Types), [NType|TypeNames])
		    end;
		false -> fill_type_table(Types, TypeNames)
	    end
    end.


build_entries(Tables, _, _, [], _) -> Tables;
build_entries(Tables, Builder, Dict, [H|T], Dicts) ->
    Data = dict:fetch(H, Dict),
    Tables_With_New_Entry = Builder(Tables, H, Data, Dict),
    build_entries(Tables_With_New_Entry, Builder, Dict, T, Dicts).

build_function_entries({Types, Symbols}, Name, Data, Dicts) ->
    {ReturnType, ArgumentList} = Data,
    Types_With_Return = build_type_entry(Types, ReturnType),
    Symbol_With_Return = dict:append(Name, {return, ReturnType}, Symbols),
    Tables = {Types_With_Return, Symbol_With_Return},
    build_arguments(Tables, Dicts, Name, 0, ArgumentList).

build_arguments(Tables, _, _, _, []) -> Tables;
build_arguments({Types, Symbols}, Dicts, FunctionName, Pos, [Arg|T]) ->
    {_, ArgType} = Arg,
    Types_With_Arg = build_type_entry(Types, ArgType),
    Symbol_With_Arg = dict:append(FunctionName, {argument, integer_to_list(Pos), ArgType}, Symbols),
    build_arguments({Types_With_Arg, Symbol_With_Arg}, Dicts, FunctionName, Pos+1, T).

build_typedef_entries({Types, Symbols}, Alias, Type, _) ->
    case lists:member(Alias, ?CLANG_BUILTINS) of
	true -> {Types, Symbols};
	false ->
	    NTypes = build_type_entry(Types, Type),
	    case dict:is_key(Alias, NTypes) of
		true ->
		    case dict:fetch(Alias, NTypes) of
			{struct, _} ->
			    %% oh my, we store a constructor for a user-defined type here, no typedef
			    {NTypes, Symbols};
			_ ->
			    %% everything is ok (typedef of already stored type)
			    {dict:store(Alias, {typedef, Type}, NTypes), Symbols}
		    end;
		false ->
		    %% everything is ok 
		    {dict:store(Alias, {typedef, Type}, NTypes), Symbols}
	    end
    end.

build_struct_entries({Types, Symbols}, Alias, _, Dict) ->
    STypes = dict:store("struct "++Alias++" *", {userdef,["*", Alias]}, Types),
    Members =  dict:fetch(Alias, Dict),
    {NTypes, Fields} = build_fields(lists:reverse(Members),[],0, STypes),
    {dict:store(Alias, {struct, Fields}, NTypes), Symbols}.

build_fields([], Fields, _, Types) -> {Types, lists:reverse(Fields)};
build_fields([{Name, Type}|T], Fields, I, Types) ->
    NTypes = build_type_entry(Types, Type),
    build_fields(T, [{field,Name, Type, I}|Fields], I+1, NTypes).

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
parse_type([], TypeDef, Kind) -> {TypeDef, Kind};
parse_type([E|T], TypeDef, Kind) ->
    case E of
	%% special cases
	"struct" ->
	    [StructName|TT] = T,
	    parse_type(TT, [StructName|TypeDef], userdef);
	%% "union" ->
	%%     io:format("TODO Parse Union ~n");
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


build_type_entry(TypeTable, Type) ->
    case dict:is_key(Type, TypeTable) of
	true -> TypeTable;
	false->
	    case parse_type(string:tokens(type_extend(Type), " ")) of
		%%case parse_type(string:tokens(Type, " "), Dicts) of
		{Def, base} ->
		    %% io:format("~p -> ~p base~n", [Type, Def]),
		    dict:store(Type, {base, Def}, TypeTable);
		{Def, userdef} ->
		    %% io:format("~p -> ~p userdef~n", [Type, Def]),
		    dict:store(Type, {userdef, Def}, TypeTable)
	    end
    end.
