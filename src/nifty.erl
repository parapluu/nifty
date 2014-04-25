-module(nifty).
-export([%% create modules
	 compile/3,
	 %% strings
	 list_to_cstr/1,
	 cstr_to_list/1,
	 %% pointers
	 dereference/1,
	 pointer/0,
	 pointer/1,
	 pointer_of/2,
	 pointer_of/1,
	 as_type/3,
	 %% memory allocation
	 mem_write/1,
	 mem_write/2,
	 mem_read/2,
	 mem_alloc/1,
	 free/1,
	 %% configuration
	 get_config/0,
	 get_env/0,
	 %% builtin types
	 get_types/0
	]).

-on_load(init/0).

-type reason() :: atom().
-type addr() :: integer().
-type ptr() :: {addr(), nonempty_string()}.
-type options() :: proplists:proplist().

init() -> %% loading code from jiffy
    PrivDir = case code:priv_dir(?MODULE) of
		  {error, _} ->
		      EbinDir = filename:dirname(code:which(?MODULE)),
		      AppPath = filename:dirname(EbinDir),
		      filename:join(AppPath, "priv");
		  Path ->
		      Path
	      end,
    ok = erlang:load_nif(filename:join(PrivDir, "nifty"), 0),
    load_dependencies().

load_dependencies() ->
    ok = load_dependency(rebar),
    ok = load_dependency(erlydtl).

load_dependency(Module) ->		    
    case code:load_file(Module) of
	{error, nofile} ->
	    %% module not found
	    NiftyPath = code:lib_dir(nifty, deps),
	    case code:add_patha(filename:join([NiftyPath, atom_to_list(Module), "ebin"])) of
		{error, _} ->
		    {error, dependencie_not_found};
		true ->
		    ok
	    end;
	{module, Module} ->
	    ok
    end.    

%% @doc Generates a NIF module out of a C header file and compiles it, 
%% generating wrapper functions for all functions present in the header file. 
%% <code>InterfaceFile</code> specifies the header file. <code>Module</code> specifies 
%% the module name of the translated NIF. <code>Options</code> specifies the compile
%% options. These options are equivalent to rebar's config options.
-spec compile(string(), module(), options()) -> 'ok' | 'fail'.
compile(InterfaceFile, Module, Options) ->
    nifty_compiler:compile(InterfaceFile, Module, Options).

%% @doc Returns nifty's base types as a dict
-spec get_types() -> dict:dict().
get_types() ->
    %% builtin types:
    %%  int types ( [(short|long)] [(long|short)] int; [(signed|unsigned)] char )
    %%  float types ( float; double)
    %%  string (char *)
    %%  pointer (void *)
    dict:from_list(
      [{"signed char",{base,["char","signed","none"]}},
       {"char",{base,["char","signed","none"]}},
       {"unsigned char",{base,["char","unsigned","none"]}},
       {"short",{base,["int","signed","short"]}},
       {"unsigned short",{base,["int","unsigned","short"]}},
       {"int",{base,["int","signed","none"]}},
       {"unsigned int",{base,["int","unsigned","none"]}},
       {"long",{base,["int","signed","long"]}},
       {"unsigned long",{base,["int","unsigned","long"]}},
       {"long long",{base,["int","signed","longlong"]}},
       {"unsigned long long",{base,["int","unsigned","longlong"]}},
       {"float",{base,["float","signed","none"]}},
       {"double",{base,["double","signed","none"]}},
       %% pointers
       {"signed char *",{base,["*","char","signed","none"]}},
       {"char *",{base,["*","char","signed","none"]}},
       {"unsigned char *",{base,["*","char","unsigned","none"]}},
       {"short *",{base,["*","int","signed","short"]}},
       {"unsigned short *",{base,["*","int","unsigned","short"]}},
       {"int *",{base,["*","int","signed","none"]}},
       {"unsigned int *",{base,["*","int","unsigned","none"]}},
       {"long *",{base,["*","int","signed","long"]}},
       {"unsigned long *",{base,["*","int","unsigned","long"]}},
       {"long long *",{base,["*","int","signed","longlong"]}},
       {"unsigned long long *",{base,["*","int","unsigned","longlong"]}},
       {"float *",{base,["*","float","signed","none"]}},
       {"double *",{base,["*","double","signed","none"]}},
       %% special types
       {"void *",{base,["*","void","signed","none"]}},
       {"char *",{base,["*","char","signed","none"]}}
      ]).

get_derefed_type(Type, Module) ->
    Types = Module:get_types(),
    ResType = nifty_typetable:resolve_type(Type, Types),
    {_, TypeDef} = dict:fetch(ResType, Types),
    [H|_] = TypeDef,
    case (H=:="*") orelse (string:str(H, "[")>0) of
	true -> 
	    [_|Token] = lists:reverse(string:tokens(ResType, " ")),
	    NType = string:join(lists:reverse(Token), " "),
	    ResNType = nifty_typetable:resolve_type(NType, Types),
	    case dict:is_key(ResNType, Types) of
		true ->
		    {_, DTypeDef} = dict:fetch(ResNType, Types),
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

%% @doc Dereference a nifty pointer
-spec dereference(ptr()) -> ptr() | integer() | float() | list() | {string(), integer()} | {'error', reason()}.
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
		{pointer, PType} ->
		    {raw_deref(Address), ModuleName++"."++PType};
		{final, DType} ->
		    build_type(Module, DType, Address);
		undef ->
		    {error, undefined}
	    end
    end.

build_builtin_type(DType, Address) ->
    case DType of
	"void *" -> {raw_deref(Address), "undef"};
	"char *" -> cstr_to_list({Address, "nifty.char *"});
	_ -> build_type(nifty, DType, Address)
    end.

build_type(Module, Type, Address) ->
    Types = Module:get_types(),
    case dict:is_key(Type, Types) of
	true -> 
	    {Kind, Def} =  dict:fetch(Type, Types),
	    case Kind of
		userdef ->
		    [Name] = Def,
		    RR = dict:fetch(Name, Types),
		    case  RR of
			{struct, _} -> 
			    Module:erlptr_to_record({Address, Name});
			_ -> 
			    {error, undef}
		    end;
		base ->
		    case Def of
			["*", "char", Sign, _] ->
			    int_deref(Address, 1, Sign);
			["*", "int", Sign, L] ->
			    {_, {ShI, I, LI, LLI, _, _}} = proplists:lookup("sizes", get_config()),
			    Size = case L of
				       "short" ->
					   ShI;
				       "none" ->
					   I;
				       "long" ->
					   LI;
				       "longlong" ->
					   LLI
				   end,
			    int_deref(Address, Size, Sign);
			["*", "float", _, _] ->
			    float_deref(Address);
			["*", "double", _, _] ->
			    double_deref(Address);
			[H|_] ->
			    case H of
				$* ->
				    {error, unknown_builtin_type};
				_ ->
				    {error, not_a_pointer}
			    end
		    end;
		_ ->
		    {error, unknown_type}
	    end;
	false ->
	    %% check if the type is a pointer and try
	    %% to dereference the type "blind"
	    case string:right(Type, 1) of
		"*" ->
		    %% pointer
		    {_, Size} = proplists:get_value("arch", nifty:get_config()),
		    NAddr = int_deref(Address, Size, "unsigned"),
		    {NAddr, atom_to_list(Module)++"."++string:left(Type, length(Type)-1)};
		_ ->
		    {error, unknown_type}
	    end
    end.

int_deref(Addr, Size, Sign) ->
    I = int_deref(lists:reverse(mem_read({Addr, "nifty.void *"}, Size)), 0),
    case Sign of
	"signed" ->
	    case I > (trunc(math:pow(2, (Size*8)-1))-1) of
		true -> 
		    I - trunc(math:pow(2,(Size*8)));
		false ->
		    I
	    end;
	"unsigned" ->
	    I
    end.

int_deref([], Acc) -> Acc;
int_deref([E|T], Acc) ->
    int_deref(T, (Acc bsl 8) + E).

%% @doc Free's the memory associated with a nifty pointer
-spec free(ptr()) -> 'ok'.
free({Addr, _}) ->
    raw_free(Addr).

%%% NIF Functions
raw_free(_) ->
    erlang:nif_error(nif_library_not_loaded).

float_deref(_) ->
    erlang:nif_error(nif_library_not_loaded).

float_ref(_) ->
    erlang:nif_error(nif_library_not_loaded).

double_deref(_) ->    
    erlang:nif_error(nif_library_not_loaded).

double_ref(_) ->    
    erlang:nif_error(nif_library_not_loaded).

%% @doc Converts an erlang string into a 0 terminated C string and returns a nifty pointer to it
-spec list_to_cstr(string()) -> ptr().
list_to_cstr(_) ->
    erlang:nif_error(nif_library_not_loaded).
%% @doc Converts a nifty pointer to a 0 terminated C string into a erlang string.
-spec cstr_to_list(ptr()) -> string().
cstr_to_list(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% size of a base type
size_of(Type) -> 
    Types = get_types(),
    case dict:fetch(Type, Types) of
	{base, ["char", _, _]} ->
	    1;
	{base, ["int", _, L]} ->
	    {_, {ShI, I, LI, LLI, _, _}} = proplists:lookup("sizes", get_config()),
	    case L of
		"short" ->
		    ShI;
		"none" ->
		    I;
		"long" ->
		    LI;
		"longlong" ->
		    LLI
	    end;
	{base, ["float", _, _]}->
	    {_, {_, _, _, _, Fl, _}} = proplists:lookup("sizes", get_config()),
	    Fl;
	{base, ["double", _, _]}->
	    {_, {_, _, _, _, _, Dbl}} = proplists:lookup("sizes", get_config()),
	    Dbl;
	{base, ["*"|_]} ->
	    {_, {_, P}} = proplists:lookup("arch", get_config()),
	    P
    end.
    
%% @doc Returns a pointer to a memory area that is the size of a pointer
-spec pointer() -> ptr().
pointer() ->
    {_, Size} = proplists:get_value("arch", nifty:get_config()),
    mem_alloc(Size).

%% @doc Returns a pointer to the specified <code>Type</code>. This function allocates memory of <b>sizeof(</b><code>Type</code><b>)</b>
-spec pointer(nonempty_string()) -> ptr() | undefined.
pointer(Type) ->
    Types = get_types(),
    case dict:is_key(Type, Types) of
	true ->
	    Size = size_of(Type),
	    as_type(mem_alloc(Size), nifty, Type++" *");
	false ->
	    case string:tokens(Type, ".") of
		["nifty", TypeName] ->
		    %% builtin type
		    pointer(TypeName);
		[ModuleName, TypeName] ->
		    Mod = list_to_atom(ModuleName),
		    case code:load_file(Mod) of
			{module, Mod} ->
			    case proplists:is_defined(get_types, Mod:module_info(exports)) of
				true ->
				    %% resolve and build
				    RType = nifty_typetable:resolve_type(TypeName, Mod:get_types()),
				    case pointer(RType) of
					undefined ->
					    case Mod:new(RType) of
						undefined ->
						    undefined;
						Value ->
						    pointer_of(Value, Type)
					    end;
					Ptr ->
					    Ptr
				    end;
				_ ->
				    undefined
			    end;
			_ -> 
			    undefined
		    end;
		_ ->
		    undefined
	    end
    end.

%% @doc Returns a pointer to the given pointer
-spec pointer_of(ptr()) -> ptr() | undefined.
pointer_of({_, Type} = Ptr) ->
    pointer_of(Ptr, Type).

%% @doc Returns a pointer to the <code>Value</code> with the type <code>Type</code>
-spec pointer_of(term(), string()) -> ptr() | undefined.
pointer_of(Value, Type) ->
    case string:right(Type, 1) of
	"*" ->
	    %% pointer
	    {Addr, VType} = Value,
	    case VType=:=Type of
		true ->
		    {_, Size} = proplists:get_value("arch", nifty:get_config()),
		    {NAddr, _} = int_constr(Addr, Size),
		    {NAddr, Type++"*"};
		false ->
		    undefined
	    end;
	_ ->
	    %% something else
	    case string:tokens(Type, ".") of
		[_] ->
		    %% base types
		    builtin_pointer_of(Value, Type);
		["nifty", T] ->
		    %% base type
		    builtin_pointer_of(Value, T);
		[ModuleName, T] ->
		    case builtin_pointer_of(Value, T) of
			undefined ->
			    %% no base type, try the module
			    %% resolve type and try again
			    Module = list_to_atom(ModuleName),
			    Types = Module:get_types(),
			    case nifty_typetable:resolve_type(T, Types) of
				undef ->
				    %% can (right now) only be a struct
				    Module:record_to_erlptr(Value);
				ResT ->
				    case builtin_pointer_of(Value, ResT) of
					undefined ->
					    %% can (right now) only be a struct
					    Module:record_to_erlptr(Value);
					Ptr ->
					    Ptr
				    end
			    end;
			Ptr ->
			    Ptr
		    end
	    end
    end.

builtin_pointer_of(Value, Type) ->
    Types = get_types(),
    case dict:is_key(Type, Types) of
	true ->
	    case dict:fetch(Type, Types) of
		{base, ["float", _, _]}->
		    float_ref(Value);
		{base, ["double", _, _]}->
		    double_ref(Value);
		_ -> case size_of(Type) of
			 undefined ->
			     undefined;
			 Size ->
			     case is_integer(Value) of
				 true ->
				     as_type(int_constr(Value, Size), nifty, Type++" *");
				 false ->
				     undefined
			     end
		     end
	    end;
	false ->
	    undefined
    end.

int_constr(Value, Size) ->
    mem_write(int_constr(Value, Size, [])).

int_constr(_, 0, Acc) ->
    lists:reverse(Acc);
int_constr(Val, S, Acc) ->
    R = Val rem 256,
    V = Val div 256,
    int_constr(V, S-1, [R|Acc]).

raw_deref(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Writes the <code>Data</code> to the memory area pointed to by <code>Ptr</code> and returns a the pointer; the list elements are interpreted as byte values
-spec mem_write(ptr(), binary() | list()) -> ptr().
mem_write({Addr, _} = Ptr, Data) ->
    {Addr, _} = case is_binary(Data) of
	true ->
	    mem_write_binary(Data, Ptr);
	false ->
	    mem_write_list(Data, Ptr)
	end,
    Ptr.

%% @doc Writes the <code>Data</code> to memory and returns a nifty pointer to it; the list elements are interpreted as byte values
-spec mem_write(binary() | list()) -> ptr().
mem_write(Data) ->
    case is_binary(Data) of
	true ->
	    mem_write_binary(Data, mem_alloc(byte_size(Data)));
	false ->
	    mem_write_list(Data, mem_alloc(length(Data)))
    end.

-spec mem_write_list(list(), ptr()) -> ptr().
mem_write_list(_, _) ->
    erlang:nif_error(nif_library_not_loaded).

-spec mem_write_binary(binary(), ptr()) -> ptr().
mem_write_binary(_, _) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Reads <code>X2</code> bytes from the pointer <code>X1</code> and returns it as list
-spec mem_read(ptr(), integer()) -> list().
mem_read(_, _) ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Allocates <code>X1</code> bytes and returns a pointer to it
-spec mem_alloc(non_neg_integer()) -> ptr().
mem_alloc(_) ->
    erlang:nif_error(nif_library_not_loaded).

%% config
%% @doc Returns the platform specific configuration of nifty
-spec get_config() -> proplists:proplist().
get_config() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Returns erlangs NIF environment
-spec get_env() -> {integer(), nonempty_string()}.
get_env() ->
    erlang:nif_error(nif_library_not_loaded).

%% @doc Casts a pointer to a <code>Type</code> of a <code>Module</code>; returns an error if the module does not specify the type
-spec as_type(ptr(), atom(), nonempty_string()) -> ptr() | undef.
as_type({Address, _}, Module, Type) ->
    case dict:is_key(Type, Module:get_types()) of
	true -> 
	    {Address, atom_to_list(Module)++"."++Type};
	false ->
	    undef
    end.
