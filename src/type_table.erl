-module(type_table).
-export([build/2]).

build({Functions, Typedefs, Structs}) ->
	% returns a symbol table and a type table
	ok.

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
