-module(nifty_typetable).
-export([build/1,
	 check_type/2,
	 check_tables/2,
	 resolve_type/2]).

-define(BASE_TYPES, ["char", "int", "float", "double", "void"]).
-define(SPECIFIER, ["signed", "unsigned", "short", "long"]).
-define(CLANG_BUILTINS, ["__int128_t", "__builtin_va_list", "__uint128_t"]).
-define(CLANG_BLACKLIST, ["__builtin_va_list"]).

-type ctype() :: string().
-type field() :: {field, string(), ctype(), integer()}.
-type ctypedef() :: {'base', [string()]} | {'typedef', string()} | {'userdef' , [string() | {constructor_type(), nonempty_string()}]}.
-type type_table() :: dict:dict(ctype(), ctypedef()).
-type argument_index() :: integer().
-type symbol_table() :: dict:dict(string(), [{'return', ctype()} | {'argument', argument_index(), ctype()}]).
-type constructor_type() :: 'struct'.
-type constructor_table() :: dict:dict({constructor_type(), nonempty_string()} , [field()]).
-type tables() :: {type_table(), symbol_table(), constructor_table()}.

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

-spec check_tables(nifty_clangparse:defs(), tables()) -> {tables(), [nonempty_string()]}.
check_tables(Defs, {Types, Symbols, Constructors}) ->
    {NDefs, NTypes} = check_types(Defs, Types),
    NConstructors = check_constructors(NDefs, Constructors),
    {NSymbols, Lost} = check_symbols(NDefs, Symbols),
    {{NTypes, NSymbols, NConstructors}, Lost}.

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

-spec check_types(nifty_clangparse:defs(), type_table()) -> {nifty_clangparse:defs(), type_table()}.
check_types(Defs, Types) ->
    {NDefs, NTypes} = check_types_once(Defs, Types),
    case length(dict:fetch_keys(Types)) =:= length(dict:fetch_keys(NTypes)) of
	true ->
	    {NDefs, NTypes};
	false ->
	    check_types(NDefs, NTypes)
    end.

-spec check_constructors(nifty_clangparse:defs(), constructor_table()) -> constructor_table().
check_constructors({_, _, Structs}, Constructors) ->
    Keys = dict:fetch_keys(Structs),
    check_constructors(Keys, Structs, Constructors, dict:new()).
    
check_constructors([], _, _, Acc) ->
    Acc;
check_constructors([H|T], Ref, Old, New) ->
    case dict:is_key(H, Ref) of
	true ->
	    Data = dict:fetch({struct, H}, Old),
	    check_constructors(T, Ref, Old, dict:store({struct, H}, Data, New));
	false ->
	    check_constructors(T, Ref, Old, New)
    end.

check_types_once({Functions, Typedefs, Structs} = Dicts, Types) ->
    NTypes = check_types_types(Dicts, Types),
    {NStructs, NNTypes} = check_types_structs(Dicts, Structs, NTypes),
    NFunc = check_types_functions(Dicts, Functions, NNTypes),
    {{NFunc, Typedefs, NStructs}, NNTypes}.

-spec check_type(ctype(), type_table()) -> boolean().
check_type(Type, Types) ->
    check_type(Type, Types, dict:new()).

check_type(Type, Types, Structs) ->
     check_type2(Type, nifty:get_types(), dict:new()) orelse check_type2(Type, Types, Structs).

check_type2(Type, Types, Structs) ->
    case dict:is_key(Type, Types) of
	true ->
	    case resolve_type(Type, Types) of
		undef -> false;
		RType ->
		    case dict:fetch(RType, Types) of
			{userdef, [RType]} ->
			    false; %% loop
			{userdef, [T]} ->
			    %% constructor or dead end
			    case T of
				{_, Name} ->
				    dict:is_key(Name, Structs);
				    %% true; %% constructor is in constructor table
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

check_types_functions(Dicts, Functions, Types) ->
    Names = dict:fetch_keys(Functions),
    check_types_functions(Dicts, Names, Functions, dict:new(), Types).

check_types_functions(_, [], _, NFunc, _) ->
    NFunc;
check_types_functions({_, _, Structs} = Dicts, [Func|Tail],OldFunc,NewFunc,Types) ->
    {RetType, ArgList} = dict:fetch(Func, OldFunc),
    case check_type(RetType, Types, Structs) andalso check_types_list(Dicts, ArgList, Types) of
	true ->
	    check_types_functions(Dicts, 
				  Tail, 
				  OldFunc, 
				  dict:store(Func,
					     dict:fetch(Func, OldFunc),
					     NewFunc),
				  Types);
	false ->
	    check_types_functions(Dicts, Tail, OldFunc, NewFunc, Types)
    end.
    
check_types_structs(Dicts, Structs, Types) ->
    Names = dict:fetch_keys(Structs),
    check_types_structs(Dicts, Names, Structs, dict:new(), Types).

check_types_structs(_, [], _, NewStructs, Types) ->
    {NewStructs,Types};
check_types_structs(Dicts, [Struct|Tail], OldStructs, NewStructs, Types) ->
    case check_types_fields(Dicts, dict:fetch(Struct, OldStructs), Types) of
	true ->
	    check_types_structs(Dicts,
				Tail, 
				OldStructs, 
				dict:store(Struct, 
					   dict:fetch(Struct, OldStructs), 
					   NewStructs),
				Types);
	false ->
	    check_types_structs(Dicts, 
				Tail,
				OldStructs,
				NewStructs,
				dict:erase("struct "++Struct, Types))
    end.
    
check_types_fields(_, [], _) ->
    false;
check_types_fields(Dicts, Fields, Types) ->
    check_types_list(Dicts, Fields, Types).

check_types_list(_, [], _) ->
    true;
check_types_list({_, _, Structs} = Dicts, [{_, Type}|Tail], Types) ->
    check_type(Type, Types, Structs) andalso check_types_list(Dicts, Tail, Types).
    
check_types_types(Dicts, Types) ->
    Names = dict:fetch_keys(Types),
    check_types_types(Dicts, Names, Types, dict:new()).

check_types_types(_, [], _, NewTypes) ->
    NewTypes;
check_types_types({_, _, Structs} = Dicts, [Type|Tail], OldTypes, NewTypes) ->
    case check_type(Type, OldTypes, Structs) of
	true ->
	    check_types_types(Dicts,
			      Tail, 
			      OldTypes, 
			      dict:store(Type, 
					 dict:fetch(Type, OldTypes), 
					 NewTypes));
	false ->
	    check_types_types(Dicts, Tail, OldTypes, NewTypes)
    end.

%% @doc builds a typetable and symbol table out of type information
-spec build(nifty_clangparse:defs()) -> {{type_table(), symbol_table(), constructor_table()}, [nonempty_string()]}.
build({Functions, Typedefs, Structs} = Dicts) ->
    Empty_Tables = {dict:new(), dict:new(), dict:new()}, % { Types, Symbols, Constructors }
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
    {Types, Symbols, Constructors} = build_entries(Tables_With_TypeDefs,
				     fun build_struct_entries/4,
				     Structs,
				     dict:fetch_keys(Structs),
				     Dicts),
    check_tables(Dicts, {fill_type_table(Types), Symbols, Constructors}).

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
			    [P|Token] = lists:reverse(string:tokens(Type, " ")),
			    NewP = string:substr(P, 1, length(P) - length(H)),
			    NType = string:strip(string:join(lists:reverse(Token)++[NewP], " ")),
			    case dict:is_key(NType, Types) of 
				true -> fill_type_table(Types, TypeNames);
				false -> fill_type_table(dict:store(NType, {Kind, T}, Types), [NType|TypeNames])
			    end;
			false -> fill_type_table(Types, TypeNames)
		    end
	    end
    end.


build_entries(Tables, _, _, [], _) -> Tables;
build_entries(Tables, Builder, Dict, [H|T], Dicts) ->
    Data = dict:fetch(H, Dict),
    Tables_With_New_Entry = Builder(Tables, H, Data, Dict),
    build_entries(Tables_With_New_Entry, Builder, Dict, T, Dicts).

build_function_entries({Types, Symbols, Constructors}, Name, Data, Dicts) ->
    {ReturnType, ArgumentList} = Data,
    Types_With_Return = build_type_entry(Types, ReturnType),
    Symbol_With_Return = dict:append(Name, {return, ReturnType}, Symbols),
    Tables = {Types_With_Return, Symbol_With_Return, Constructors},
    build_arguments(Tables, Dicts, Name, 0, ArgumentList).

build_arguments(Tables, _, _, _, []) -> Tables;
build_arguments({Types, Symbols, Constructors}, Dicts, FunctionName, Pos, [Arg|T]) ->
    {_, ArgType} = Arg,
    Types_With_Arg = build_type_entry(Types, ArgType),
    Symbol_With_Arg = dict:append(FunctionName, {argument, integer_to_list(Pos), ArgType}, Symbols),
    build_arguments({Types_With_Arg, Symbol_With_Arg, Constructors}, Dicts, FunctionName, Pos+1, T).

build_typedef_entries({Types, Symbols, Constructors}, Alias, Type, _) ->
    case lists:member(Alias, ?CLANG_BUILTINS) of
	true -> {Types, Symbols};
	false ->
	    NTypes = build_type_entry(Types, Type),
	    case dict:is_key(Alias, NTypes) of
		true ->
		    case dict:fetch(Alias, NTypes) of
			{struct, _} ->
			    %% oh my, we store a constructor for a user-defined type here, no typedef
			    {NTypes, Symbols, Constructors};
			_ ->
			    %% everything is ok (typedef of already stored type)
			    {dict:store(Alias, {typedef, Type}, NTypes), Symbols, Constructors}
		    end;
		false ->
		    %% everything is ok 
		    {dict:store(Alias, {typedef, Type}, NTypes), Symbols, Constructors}
	    end
    end.

build_struct_entries({Types, Symbols, Constructors}, Alias, _, Dict) ->
    STypes = dict:store("struct "++Alias++" *", {userdef,["*", {struct, Alias}]}, Types),
    Members =  dict:fetch(Alias, Dict),
    {NTypes, Fields} = build_fields(lists:reverse(Members),[],0, STypes),
    {NTypes, Symbols, dict:store({struct, Alias}, Fields, Constructors)}.

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
	    parse_type(TT, [{struct, StructName}|TypeDef], userdef);
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

norm_type(Type) ->
    case string:str(Type, "[]") of
	0 -> Type;
	P -> string:substr(Type, 1, P-1)
    end.

build_type_entry(TypeTable, Type) ->
    NType = norm_type(Type),
    case dict:is_key(NType, TypeTable) of
	true -> TypeTable;
	false->
	    case parse_type(string:tokens(type_extend(NType), " ")) of
		%%case parse_type(string:tokens(Type, " "), Dicts) of
		{Def, base} ->
		    %% io:format("~p -> ~p base~n", [Type, Def]),
		    dict:store(NType, {base, Def}, TypeTable);
		{Def, userdef} ->
		    %% io:format("~p -> ~p userdef~n", [Type, Def]),
		    dict:store(NType, {userdef, Def}, TypeTable)
	    end
    end.
