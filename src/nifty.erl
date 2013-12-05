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
	[{_, [H|_]}] = dict:fetch(Type, Types),
	 case (H=:="*") orelse string:str(H, "[") of
		 true -> 
			[_|Token] = lists:reverse(string:tokens(Type, " ")),
			NType = string:join(lists:reverse(Token), " "),
			case get_derefed_type(NType, Module) of
				fail -> {final, NType};
				_    -> {pointer, NType}
			end;
		_ -> 
			fail
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
		{final, NType} ->
			%% do something smart
			ok;
		_ -> 
			Module:build_type(Address, Type)
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

