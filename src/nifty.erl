-module(nifty).
-export([
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
	 mem_write/2,
	 mem_writer/2,
	 mem_read/2,
	 mem_alloc/1,
	 get_config/0,
	 get_env/0,
	 as_type/3,
	 get_types/0
	]).

-on_load(init/0).

init() -> %% loading code from jiffy
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, _} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
		      AppPath = filename:dirname(EbinDir),
		      filename:join(AppPath, "priv");
		  Path ->
		      Path
	      end,
    erlang:load_nif(filename:join(PrivDir, "nifty"), 0).

get_types() ->
    dict:from_list(
      [{"char *", none},
       {"void *", none}]
     ).

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
	true ->
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
		{pointer, NType} ->
		    {raw_deref(Address), ModuleName++"."++NType};
		{final, DType} ->
		    build_type(Module, DType, Address)
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

-type addr() :: integer().
-type pointer() :: {addr(), nonempty_string()}.

-spec free(pointer()) -> 'ok'.
free({Addr, _}) ->
    raw_free(Addr).

%%% NIF Functions
-spec raw_free(addr()) -> 'ok'.
raw_free(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% string conversion
list_to_cstr(_) ->
    erlang:nif_error(nif_library_not_loaded).

cstr_to_list(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% pointer arithmetic
pointer() ->
    {_, Size} = proplists:get_value("arch", nifty:get_config()),
    mem_alloc(Size).

%% pointer(Type) ->
%%     %% add sizof function to modules
%%     ok.

raw_pointer_of(_) ->
    erlang:nif_error(nif_library_not_loaded).

raw_deref(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% memory operation
-spec mem_write(list()|binary()) -> pointer().
mem_write(Data) ->
    case erlang:is_binary(Data) of
	true ->
	    mem_writer(Data, mem_alloc(size(Data)));
	false ->
	    mem_write(Data, mem_alloc(length(Data)))
    end.

-spec mem_write(binary(), pointer()) -> pointer().
mem_write(_,_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec mem_writer(list(), pointer()) -> pointer().
mem_writer(_,_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec mem_read(pointer(), integer()) -> list().
mem_read(_,_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec mem_alloc(non_neg_integer()) -> pointer().
mem_alloc(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% config
get_config() ->
    exit(nif_library_not_loaded).

get_env() ->
    exit(nif_library_not_loaded).

as_type({Address, _}, Module, Type) ->
    case dict:is_key(Type, Module:get_types()) of
	true -> 
	    {Address, erlang:atom_to_list(Module)++"."++Type};
	false ->
	    undef
    end.
