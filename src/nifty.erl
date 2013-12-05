-module(nifty).
-export([
	generate/4,
	dereference/1,
	free/1,
	%% nif functions
	raw_deref/1,
	raw_free/1, 
	list_to_cstr/1,
	cstr_to_list/1
	]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("nifty", 0).

generate(Header, Module, CompileOptions, TemplatePath) ->
    io:format("processing ~s -> ~s ~s ~n", [Header, Module++"_nif.c", Module++".erl"]),
    %% c parse stuff
    PathToH = Header,
    {Token, _} = clang_parse:parse([PathToH|CompileOptions]),
    {Functions, Typedefs, Structs} = clang_parse:build_vars(Token),
    {Types, Symbols} = type_table:build({Functions, Typedefs, Structs}),
    %% template stuff
    CTemplate = erlang:list_to_atom("C"++Header),
    ETemplate = erlang:list_to_atom("E"++Header),
    ok = erlydtl:compile(
	   filename:join([TemplatePath,"templates/cmodule.tpl"]),
	   CTemplate,
	   [{force_recompile, true},
	    {custom_tags_modules, [nifty_tags]},
	    {custom_filters_modules, [nifty_filters]}]),
    ok = erlydtl:compile(
	   filename:join([TemplatePath,"templates/emodule.tpl"]),
	   ETemplate,
	   [{force_recompile, true},
	    {custom_tags_modules, [nifty_tags]},
	    {custom_filters_modules, [nifty_filters]}]),
    RenderVars = [
		  {"functions", Functions},  % ?
		  {"structs", Structs},      % ?
		  {"typedefs", Typedefs},    % ? 
		  {"module", Module},
		  {"header", Header},
		  {"types", Types},
		  {"symbols", Symbols},
		  {"none", none}
		 ],
    {ok, COutput} = CTemplate:render(RenderVars),
    {ok, EOutput} = ETemplate:render(RenderVars),
    {EOutput, COutput}.

get_derefed_type(Type, Module) ->
	Types = Module:get_types(),
	ResType = resolve_type(Type, Types),
	[{_, TypeDef}] = dict:fetch(ResType, Types),
	[H|_] = TypeDef,
	case (H=:="*") orelse (string:str(H, "[")>0) of
		true -> 
			[_|Token] = lists:reverse(string:tokens(ResType, " ")),
			NType = string:join(lists:reverse(Token), " "),
			ResNType = resolve_type(NType, Types),
			[{_, DTypeDef}] = dict:fetch(ResNType, Types),
			[DH|_] = DTypeDef,
			case (DH=:="*") orelse (string:str(DH, "[")>0) of
				true -> {pointer, ResNType};
				false -> {final, ResNType}
			end;
		false ->
			{final, ResType}
	end.

resolve_type(Type, Types) ->
	[{Kind, TypeDef}] = dict:fetch(Type, Types),
	case Kind of
		typedef -> resolve_type(TypeDef, Types);
		_ -> Type
	end.


%% pointer arithmetic
dereference(Pointer) ->
	{Address, Module, Type} = Pointer,
	NType = get_derefed_type(Type, Module),
	case NType of
		fail ->
			erlang:error(badpointer);
		{pointer, NType} ->
			{raw_deref(Address), Module, NType};
		{final, DType} ->
			build_type(Module, DType, Address);
		_ -> 
			undef
	end.

build_type(Module, Type, Address) ->
	Types = Module:get_types(),
	[{Kind, Def}] = dict:fetch(Type, Types),
	case Kind of
		userdef ->
			[Name] = Def,
			[RR] = dict:fetch(Name, Types),
			case  RR of
				{struct, _} -> 
					Module:erlptr_to_record({Address, Module, Name});
				_ -> 
					undef
			end;
		base ->
			io:format("Base Type conversion~n"),
			ok;
		_ ->
			undef
	end.

free({Addr, _, _}) ->
	raw_free(Addr).

%%% NIF Functions
raw_deref(_) ->
	exit(nif_library_not_loaded).

raw_free(_) ->
	exit(nif_library_not_loaded).

%% string conversion
list_to_cstr(_) ->
	exit(nif_library_not_loaded).

cstr_to_list(_) ->
	exit(nif_library_not_loaded).

