-module(type_table).
-export([build/2]).

build({Functions, Typedefs, Structs}) ->
	Empty_Tables = {dict:new(), dict:new()}, % { Types, Symbols }
	Tables_With_Functions = build_entries(
		Empty_Tables,
		fun build_function_entries/3,
		Functions,
		dict:fetch_keys(Functions)),
	Tables_With_TypeDefs  = build_entries(
		Tables_With_Functions,
		fun build_typedef_entries/3,
		Typedefs,
		dict:fetch_keys(Typedefs)),
	Tables_With_Structs   = build_entries(
		Tables_With_TypeDefs,
		fun build_struct_entries/3,
		Structs,
		dict:fetch_keys(Typedefs)),
	Tables_With_Structs.

check_types(Tables) -> 
	% check if every type is resolvable to a base type
	ok.

build_entries(Tables, Builder, Dict, [H|T]) ->
	[Data] = dict:fetch(H, Dict),
	Tables_With_New_Entry = Builder(Tables, H, Data),
	build_entries(Tables_With_New_Entry, Functions, T).

build_function_entries({Types, Symbols}, Name, Data) -> ok.
build_typedef_entries({Types, Symbols}, Name, Data) -> ok.
build_struct_entries({Types, Symbols}, Name, Data) -> ok.


build_type_entry(TypeTable, Type) ->
	% is a dict
	%    typename -> full typename "int **"
	%    typedef  -> ["*","*","int"] or ["*", "struct point"] with basetype at the end (or reversed
	%    kind     -> [simple, struct, typedef, ... evtl. union, enum]
	%    if struct 
	%    members  -> dict with name -> full typename
	%    if typedef
	%    typeref  -> full typename of reftype, typedef can be empty
	ok.

build_symbol_entry(TypeTable, SymbolTable, Name, Type) ->
	% is a dict
	%    symbolname -> e.a. [functionname, return] [functionname, return] [struct1name, struct2name, membername]
	%    type       -> full type name
	ok.
