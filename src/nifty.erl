-module(nifty).
-export([
	 generate/5,
	 dereference/1,
	 free/1,
	 %% nif functions
	 raw_deref/1,
	 raw_free/1, 
	 list_to_cstr/1,
	 cstr_to_list/1,
	 pointer/0,
	 raw_pointer_of/1,
	 mem_write/1,
	 mem_read/2,
	 get_config/0,
	 as_type/3,
	 get_types/0
	]).

-on_load(init/0).

init() ->
    ok = erlang:load_nif("nifty", 0).

get_types() ->
	dict:from_list(
	    [{"char *", none},
	    {"void *", none}]
	    ).

generate(Header, Module, CompileOptions, Path, Rebuild) ->
    io:format("processing ~s -> ~s ~s ~n", [Header, Module++"_nif.c", Module++".erl"]),
    %% c parse stuff
    PathToH = Header,
    case clang_parse:parse([PathToH|CompileOptions]) of
	{fail, _} -> fail;
	{Token, _} -> 
	    {Functions, Typedefs, Structs} = clang_parse:build_vars(Token),
	    {Types, Symbols} = type_table:build({Functions, Typedefs, Structs}),
	    %% template stuff
	    CTemplate = erlang:list_to_atom("NiftyCTemplate"),
	    ETemplate = erlang:list_to_atom("NiftyETemplate"),
	    case Rebuild of
		true ->
		    io:format("Compiling Templates..."),
		    ok = erlydtl:compile(
			   filename:join([Path,"templates/cmodule.tpl"]),
			   CTemplate,
			   [{out_dir, filename:join([Path,"src"])},
			    {force_recompile, true},
			    {custom_tags_modules, [nifty_tags]},
			    {custom_filters_modules, [nifty_filters]}]),
		    ok = erlydtl:compile(
			   filename:join([Path,"templates/emodule.tpl"]),
			   ETemplate,
			   [{out_dir, filename:join([Path,"src"])},
			    {force_recompile, true},
			    {custom_tags_modules, [nifty_tags]},
			    {	custom_filters_modules, [nifty_filters]}]),
		    io:format("done~n"),
		    io:format("Rendering templates...");
		false ->
		    ok
	    end,
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
	    io:format("done~n"),
	    {EOutput, COutput}
    end.

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
	    case dict:is_key(ResNType, Types) of
			true ->
				[{_, DTypeDef}] = dict:fetch(ResNType, Types),
				[DH|_] = DTypeDef,
				case (DH=:="*") orelse (string:str(DH, "[")>0) of
				true -> {pointer, ResNType};
				false -> {final, ResNType}
				end;
			false ->
				undef
		end;
	false ->
	    {final, ResType}
    end.

resolve_type(Type, Types) ->
	case dict:is_key(Type, Types) of 
		true->
			[{Kind, TypeDef}] = dict:fetch(Type, Types),
			case Kind of
			typedef -> resolve_type(TypeDef, Types);
			_ -> Type
			end;
		false ->
			undef
	end.


%% pointer arithmetic
dereference(Pointer) ->
    {Address, ModuleType} = Pointer,
    [ModuleName, Type] = string:tokens(ModuleType, "."),
    Module = list_to_atom(ModuleName),
    case Module of
	nifty ->
	    build_builtin_type(Type, Address);
	_ ->
	    NType = get_derefed_type(Type, Module),
	    case NType of
		fail ->
		    erlang:error(badpointer);
		{pointer, NType} ->
		    {raw_deref(Address), Module++"."++NType};
		{final, DType} ->
		    build_type(Module, DType, Address);
			_ -> 
				       undef
			       end
    end.

build_builtin_type(DType, Address) ->
    case DType of
	"void *" -> {raw_deref(Address), "undef"};
	"char *" -> cstr_to_list({Address, "nifty.char *"});
	_ -> undef
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
		    Module:erlptr_to_record({Address, Name});
		_ -> 
		    undef
	    end;
	base ->
	    io:format("Base Type conversion~n"),
	    ok;
	_ ->
	    undef
    end.

free({Addr, _}) ->
    raw_free(Addr).

%%% NIF Functions
raw_free(_) ->
    exit(nif_library_not_loaded).

%% string conversion
list_to_cstr(_) ->
    exit(nif_library_not_loaded).

cstr_to_list(_) ->
    exit(nif_library_not_loaded).

%% pointer arithmetic
pointer() ->
    exit(nif_library_not_loaded).

raw_pointer_of(_) ->
    exit(nif_library_not_loaded).

raw_deref(_) ->
    exit(nif_library_not_loaded).

%% memory operation
mem_write(_) ->
    exit(nif_library_not_loaded).

mem_read(_,_) ->
    exit(nif_library_not_loaded).

%% config
get_config() ->
    exit(nif_library_not_loaded).

as_type({Address, _}, Module, Type) ->
    case dict:is_key(Type, Module:get_types()) of
	true -> 
	    {Address, erlang:atom_to_list(Module)++"."++Type};
	false ->
	    undef
	end.
