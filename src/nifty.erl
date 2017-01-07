%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2015-2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                     and  Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty).
-export([%% create modules
         compile/3,
         compile/2,
         %% strings
         list_to_cstr/1,
         cstr_to_list/1,
         %% pointers
         dereference/1,
         pointer/0,
         pointer/1,
         pointer_of/2,
         pointer_of/1,
         %% enums
         enum_value/2,
         %% types
         as_type/2,
         size_of/1,
         %% memory allocation
         mem_write/1,
         mem_write/2,
         mem_read/2,
         mem_alloc/1,
         mem_copy/3,
         malloc/1,
         free/1,
         %% configuration
         get_config/0,
         get_env/0,
         %% builtin types
         get_types/0,
         %% array utilities
         array_new/2,
         array_ith/2,
         array_element/2,
         array_set/3,
         list_to_array/2,
         array_to_list/2
        ]).

-export_type([error_reason/0, options/0, ptr/0, comp_ret/0, cvalue/0]).

-type error_reason() :: 'compile' | 'no_file'.
-type options() :: proplists:proplist().

-type addr() :: integer().
-type ptr() :: {addr(), nonempty_string()}.
-type cvalue_error() :: 'undefined' | 'unknown_builtin_type' | 'unknown_type'.
-type cvalue() :: ptr() | integer() | float() | tuple()
                | {string(), integer()} | {'error', cvalue_error()}.

%%---------------------------------------------------------------------------
%% Initialization of NIFs
%%---------------------------------------------------------------------------

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
  ok = erlang:load_nif(filename:join(PrivDir, "nifty"), 0),
  load_dependencies().

load_dependencies() ->
  ok = load_dependency(rebar),
  ok = load_dependency(erlydtl).

load_dependency(Module) ->
  case code:ensure_loaded(Module) of
    {error, nofile} ->
      %% module not found
      NiftyPath = code:lib_dir(nifty, deps),
      case code:add_patha(filename:join([NiftyPath, atom_to_list(Module), "ebin"])) of
        {error, _} ->
          {error, dependency_not_found};
        true ->
          ok
      end;
    {module, Module} ->
      ok
  end.

%%---------------------------------------------------------------------------
%% Compile
%%---------------------------------------------------------------------------

-type comp_ret() :: 'ok' | {'error', error_reason()} | {'warning', {'not_complete', [nonempty_string()]}}.

%% @doc same as compile(InterfaceFile, Module, []).
-spec compile(string(), module()) -> comp_ret().
compile(InterfaceFile, Module) ->
  compile(InterfaceFile, Module, []).

%% @doc Generates a NIF module out of a C header file and compiles it,
%% generating wrapper functions for all functions present in the header file.
%% <code>InterfaceFile</code> specifies the header file.
%% <code>Module</code> specifies the module name of the translated NIF.
%% <code>Options</code> specifies the compile options.
%% These options are a superset rebar's config options and include
%% additional Nifty options: <br/>
%%     <code>{nifty, NiftyOptions}</code> <br/>
%% where NiftyOptions is a list of options, which can be: <br/>
%% <table border="1">
%% <tr><td><code>schedule_dirty</code></td><td>use dirty schedulers</td></tr>
%% </table>

-spec compile(string(), module(), options()) -> comp_ret().
compile(InterfaceFile, Module, Options) ->
  nifty_rebar:init(),
  ModuleName = atom_to_list(Module),
  os:putenv("NIF", libname(ModuleName)),
  {ok, NiftyRoot} = file:get_cwd(),
  os:putenv("NIFTY_ROOT", NiftyRoot),
  UCO = update_compile_options(InterfaceFile, ModuleName, Options),
  Env = build_env(ModuleName, UCO),
  CFlags = string:tokens(proplists:get_value("CFLAGS", Env, ""), " "),
  case render(InterfaceFile, ModuleName, CFlags, UCO) of
    {error, _} = E ->
      E;
    {Output, Lost} ->
      ok = store_files(InterfaceFile, ModuleName, UCO, Output),
      case compile_module(ModuleName) of
        ok ->
          ModulePath = filename:absname(filename:join([ModuleName, "ebin"])),
          true = code:add_patha(ModulePath),
          purge_code(Module),
          case Lost of
            [] -> ok;
            _ -> {warning, {not_complete, Lost}}
          end;
        fail ->
          {error, compile}
      end
  end.

purge_code(Module) ->
  case code:is_loaded(Module) of
    false ->
      false;
    {file, _Loaded} ->
      case check_old_code(Module) of
        false ->
          code:delete(Module);
        true ->
          nop
      end,
      code:purge(Module),
      true
  end.

-type renderout() :: {iolist(), iolist(), iolist(), iolist(), iolist(), iolist()}.
-type modulename() :: string().

%% @doc Renders an <code>InterfaceFile</code> into a Erlang module
%% containing of <code>ModuleName</code>.erl
%% <code>ModuleName</code>.c, <code>ModuleName</code>.app and
%% <code>rebar</code>.config and returns the contents of these files
%% as tuple of iolists (in this order). It uses <code>CFlags</code> to
%% parse the <code>InterfaceFile</code> and <code>Options</code> to
%% compile it. <code>Options</code> are equivalent to rebar options.
-spec render(string(), modulename(), [string()], options()) -> {'error', error_reason()} | {renderout(), [nonempty_string()]}.
render(InterfaceFile, ModuleName, CFlags, Options) ->
  io:format("generating... ~n"),%%, [ModuleName, ModuleName++"_remote", InterfaceFile]),
  %% c parse stuff
  PathToH = InterfaceFile,
  case filelib:is_file(PathToH) andalso (not filelib:is_dir(PathToH)) of
    false ->
      {error, no_file};
    true ->
      case nifty_clangparse:parse([PathToH|CFlags]) of
        {error, fail} ->
          {error, compile};
        {FuncLoc, Raw_Symbols, Raw_Types, Unsave_Constructors} ->
          Constructors = check_constructors(Unsave_Constructors),
          Unsave_Types = nifty_clangparse:build_type_table(Raw_Types, Constructors),
          Types = check_types(Unsave_Types, Constructors),
          Unsave_Symbols = filter_symbols(InterfaceFile, Raw_Symbols, FuncLoc, Options),
          {Symbols, Lost} = check_symbols(Unsave_Symbols, Types),
          RenderVars = [{"module", ModuleName},
                        {"header", InterfaceFile},
                        {"config", Options},
                        {"types", Types},
                        {"symbols", Symbols},
                        {"constructors", Constructors},
                        {"none", none}],
          COutput = render_with_errors(nifty_c_template, RenderVars),
          ErlOutput = render_with_errors(nifty_erl_template, RenderVars),
          SaveErlOutput = render_with_errors(nifty_save_erl_template, RenderVars),
          HrlOutput = render_with_errors( nifty_hrl_template, RenderVars),
          AppOutput = render_with_errors(nifty_app_template, RenderVars),
          ConfigOutput = render_with_errors(nifty_config_template, RenderVars),
          {{ErlOutput, SaveErlOutput, HrlOutput, COutput, AppOutput, ConfigOutput}, Lost}
      end
  end.

render_with_errors(Template, Vars) ->
  try Template:render(Vars) of
      {ok, Output} -> Output;
      {error, Err} ->
      io:format("Error during rendering of template ~p:~n~p~nPlease report the error~n", [Template, Err]),
      throw(nifty_render_error)
  catch
    ET:E ->
      io:format("~p:~p during rendering of template ~p:~nVars: ~n~p~nPlease report the error~n", [ET, E, Template, Vars]),
      throw(nifty_render_error)
  end.


check_types(Types, Constr) ->
  %% somehow we have incomplete types in the type table
  Pred = fun (Key, Value) ->
             case Value of
               {userdef, [{struct, Name}]} ->
                 dict:is_key({struct, Name}, Constr);
               _ ->
                 nifty_types:check_type(Key, Types, Constr)
             end
         end,
  dict:filter(Pred, Types).

check_constructors(Constr) ->
  Pred = fun (_, Fields) -> length(Fields) > 0 end,
  dict:filter(Pred, Constr).

filter_symbols(InterfaceFile, Symbols, FuncLoc, Options) ->
  Pred = case get_filter_option(Options) of
           none ->
             BaseName = filename:basename(InterfaceFile),
             fun (Key, _) ->
                 filename:basename(dict:fetch(Key, FuncLoc)) =:= BaseName
             end;
           RegEx ->
             fun (Key, _) ->
                 Location = dict:fetch(Key, FuncLoc),
                 case re:run(Location, RegEx, [{capture, none}]) of
                   match -> true;
                   {error, _} -> error(nifty_bad_regex);
                   _ -> false
                 end
             end
         end,
  dict:filter(Pred, Symbols).

get_filter_option(Options) ->
  case proplists:lookup(nifty, Options) of
    {nifty, NiftyOptions} ->
      case proplists:lookup(filter_headers, NiftyOptions) of
        {filter_headers, RegEx} -> RegEx;
        _ -> none
      end;
    _ -> none
  end.


check_symbols(Symbols, Types) ->
  Pred = fun (_, Args) -> check_args(Args, Types) end,
  Accml = fun(Name, Args, AccIn) ->
              case check_args(Args, Types) of
                true -> AccIn;
                false -> [Name|AccIn]
              end
          end,
  {dict:filter(Pred, Symbols), dict:fold(Accml, [], Symbols)}.

check_args([], _) -> true;
check_args([H|T], Types) ->
  Type = case H of
           {return, Tp} -> Tp;
           {argument, _, Tp} -> Tp
         end,
  case nifty_types:check_type(Type, Types) of
    false -> false;
    true -> check_args(T, Types)
  end.

store_files(InterfaceFile, ModuleName, Options, RenderOutput) ->
  {ok, Path} = file:get_cwd(),
  store_files(InterfaceFile, ModuleName, Options, RenderOutput, Path).

store_files(_, ModuleName, _, RenderOutput, Path) ->
  ok = case file:make_dir(filename:join([Path, ModuleName])) of
         ok -> ok;
         {error,eexist} -> ok;
         _ -> fail
       end,
  ok = case file:make_dir(filename:join([Path, ModuleName, "src"])) of
         ok -> ok;
         {error,eexist} -> ok;
         _ -> fail
       end,
  ok = case file:make_dir(filename:join([Path, ModuleName, "include"])) of
         ok -> ok;
         {error,eexist} -> ok;
         _ -> fail
       end,
  ok = case file:make_dir(filename:join([Path, ModuleName, "c_src"])) of
         ok -> ok;
         {error,eexist} -> ok;
         _ -> fail
       end,
  ok = case file:make_dir(filename:join([Path, ModuleName, "ebin"])) of
         ok -> ok;
         {error,eexist} -> ok;
         _ -> fail
       end,
  {ErlOutput, SaveErlOutput, HrlOutput, COutput, AppOutput, ConfigOutput} = RenderOutput,
  ok = fwrite_render(Path, ModuleName, "src", ModuleName++".erl", ErlOutput),
  ok = fwrite_render(Path, ModuleName, "src", ModuleName++"_remote"++".erl", SaveErlOutput),
  ok = fwrite_render(Path, ModuleName, "include", ModuleName++".hrl", HrlOutput),
  ok = fwrite_render(Path, ModuleName, "c_src", ModuleName++"_nif.c", COutput),
  ok = fwrite_render(Path, ModuleName, "ebin", ModuleName++".app", AppOutput),
  ok = fwrite_render(Path, ModuleName, ".", "rebar.config", ConfigOutput).

fwrite_render(Path, ModuleName, Dir, FileName, Template) ->
  file:write_file(filename:join([Path, ModuleName, Dir, FileName]), [Template]).

compile_module(ModuleName) ->
  {ok, Path} = file:get_cwd(),
  ok = file:set_cwd(filename:join([Path, ModuleName])),
  try rebar_commands(["compile"]) of
      _ -> file:set_cwd(Path)
  catch
    throw:rebar_abort ->
      ok = file:set_cwd(Path),
      fail
  end.

rebar_commands(RawArgs) ->
  Args = nifty_rebar:parse_args(RawArgs),
  BaseConfig = nifty_rebar:init_config(Args),
  {BaseConfig1, Cmds} = nifty_rebar:save_options(BaseConfig, Args),
  nifty_rebar:run(BaseConfig1, Cmds).


build_env(ModuleName, Options) ->
  Env = case proplists:get_value(port_env, Options) of
          undefined -> [];
          EnvList -> EnvList
        end,
  EnvAll = case proplists:get_value(port_specs, Options) of
             undefined -> Env;
             SpecList ->
               lists:concat([Env, get_spec_env(ModuleName, SpecList)])
           end,
  Config = rebar_config:set(rebar_config:new(), port_env, EnvAll),
  rebar_port_compiler:setup_env(Config).

get_spec_env(_, []) -> [];
get_spec_env(ModuleName, [S|T]) ->
  Lib = libname(ModuleName),
  case S of
    {_, Lib, _, Options} ->
      case proplists:get_value(env, Options) of
        undefined -> [];
        Env -> expand_env(Env, [])
      end;
    _ ->
      get_spec_env(ModuleName, T)
  end.

norm_opts(Options) ->
  case proplists:get_value(env, Options) of
    undefined -> Options;
    Env ->
      [{env, merge_env(expand_env(Env, []), dict:new())}| proplists:delete(env, Options)]
  end.

merge_env([], D) -> dict:to_list(D);
merge_env([{Key, Opt}|T], D) ->
  case dict:is_key(Key, D) of
    true ->
      merge_env(T, dict:store(Key, dict:fetch(Key,D) ++ " " ++ remove_envvar(Key, Opt), D));
    false ->
      merge_env(T, dict:store(Key, Opt, D))
  end.

remove_envvar(Key, Opt) ->
  %% remove in the beginning and the end
  Striped = string:strip(Opt),
  K1 = "${" ++ Key ++ "}",
  K2 = "$" ++ Key,
  K3 = "%" ++ Key,
  E1 = length(Striped) - length(K1) + 1,
  E23 = length(Striped) - length(K2) + 1,
  case string:str(Striped, K1) of
    1 ->
      string:substr(Striped, length(K1)+1);
    E1 ->
      string:substr(Striped, 1, E1);
    _ ->
      case string:str(Striped, K2) of
        1 ->
          string:substr(Striped, length(K2)+1);
        E23 ->
          string:substr(Striped, 1, E23 -1);
        _ ->
          case string:str(Striped, K3) of
            1 ->
              string:substr(Striped, length(K3)+1);
            E23 ->
              string:substr(Striped, 1, E23 - 1);
            _ ->
              Striped
          end
      end
  end.

expand_env([], Acc) ->
  Acc;
expand_env([{ON, O}|T], Acc) ->
  expand_env(T, [{ON, nifty_utils:expand(O)}|Acc]).

libname(ModuleName) ->
  "priv/"++ModuleName++"_nif.so".

update_compile_options(InterfaceFile, ModuleName, CompileOptions) ->
  NewPort_Spec = case proplists:get_value(port_specs, CompileOptions) of
                   undefined ->
                     [module_spec(".*", [], [], InterfaceFile, ModuleName)];
                   UPortSpec ->
                     update_port_spec(InterfaceFile, ModuleName, UPortSpec, [], false)
                 end,
  orddict:store(port_specs, NewPort_Spec, orddict:from_list(CompileOptions)).

module_spec(ARCH, Sources, Options, InterfaceFile,  ModuleName) ->
  {
    ARCH,
    libname(ModuleName),
    ["c_src/"++ModuleName++"_nif.c"|abspath_sources(Sources)],
    norm_opts(join_options([{env,
                             [{"CFLAGS",
                               "$CFLAGS -I"++filename:absname(filename:dirname(nifty_utils:expand(InterfaceFile)))}]}],
                           Options))
  }.

join_options(Proplist1, Proplist2) ->
  orddict:merge(fun(_, X, Y) -> X ++ Y end,
                orddict:from_list(Proplist1),
                orddict:from_list(Proplist2)).

abspath_sources(S) -> abspath_sources(S, []).

abspath_sources([], Acc) -> Acc;
abspath_sources([S|T], Acc) ->
  abspath_sources(T, [filename:absname(nifty_utils:expand(S))|Acc]).


update_port_spec(_,  _, [], Acc, true) ->
  Acc;
update_port_spec(InterfaceFile,  ModuleName, [], Acc, false) -> %% empty spec
  [module_spec(".*", [], [], InterfaceFile, ModuleName), Acc];
update_port_spec(InterfaceFile,  ModuleName, [Spec|T], Acc, Found) ->
  Shared = libname(ModuleName),
  case expand_spec(Spec) of
    {ARCH, Shared, Sources} ->
      update_port_spec(
        InterfaceFile,
        ModuleName,
        T,
        [module_spec(ARCH, Sources, [], InterfaceFile, ModuleName)|Acc], true);
    {ARCH, Shared, Sources, Options} ->
      update_port_spec(
        InterfaceFile,
        ModuleName,
        T,
        [module_spec(ARCH, Sources, Options, InterfaceFile, ModuleName)|Acc], true);
    _ ->
      update_port_spec(InterfaceFile, ModuleName, T, [Spec|Acc], Found)
  end.

expand_spec(S) ->
  case S of
    {ARCH, Shared, Sources} ->
      {ARCH, nifty_utils:expand(Shared), norm_sources(Sources)};
    {ARCH, Shared, Sources, Options} ->
      {ARCH, nifty_utils:expand(Shared), norm_sources(Sources), Options}
  end.

norm_sources(S) ->
  [nifty_utils:expand(X) || X <- S].

%%---------------------------------------------------------------------------

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
     {"_Bool", {typedef, "int"}},
     %% special types
     {"void *",{base,["*","void","signed","none"]}},
     {"char *",{base,["*","char","signed","none"]}}
    ]).

get_derefed_type(Type, Module) ->
  Types = Module:get_types(),
  case dict:is_key(Type, Types) of
    true ->
      ResType = nifty_types:resolve_type(Type, Types),
      {_, TypeDef} = dict:fetch(ResType, Types),
      [H|_] = TypeDef,
      case (H =:= "*") orelse (string:str(H, "[") > 0) of
        true ->
          [[_|PointerDef]|Token] = lists:reverse(string:tokens(ResType, " ")),
          NType = case PointerDef of
                    [] ->string:join(lists:reverse(Token), " ");
                    _ -> string:join(lists:reverse([PointerDef|Token]), " ")
                  end,
          ResNType = nifty_types:resolve_type(NType, Types),
          case dict:is_key(ResNType, Types) of
            true ->
              {_, DTypeDef} = dict:fetch(ResNType, Types),
              [DH|_] = DTypeDef,
              case DH of
                {_, _} -> {final, ResNType};
                _ -> case (DH =:= "*") orelse (string:str(DH, "[") > 0) of
                       true -> {pointer, ResNType};
                       false -> {final, ResNType}
                     end
              end;
            false ->
              undef
          end;
        false ->
          {final, ResType}
      end;
    false ->
      case lists:last(Type) of
        $* ->
          %% pointer
          NName = string:strip(string:left(Type, length(Type)-1)),
          case lists:last(NName) of
            $* ->
              {pointer, NName};
            _ ->
              {final, NName}
          end;
        _ ->
          {error, unknown_type}
      end
  end.

%% @doc Dereference a nifty pointer
-spec dereference(ptr()) -> cvalue().
dereference(Pointer) ->
  {Address, ModuleType} = Pointer,
  [ModuleName, Type] = case string:tokens(ModuleType, ".") of
                         [NiftyType] -> ["nifty", NiftyType];
                         FullType -> FullType
                       end,
  Module = list_to_atom(ModuleName),
  %% case Module of
  %%  nifty ->
  %%      build_builtin_type(Type, Address);
  %%  _ ->
  NType = get_derefed_type(Type, Module),
  case NType of
    {pointer, PType} ->
      {raw_deref(Address), ModuleName++"."++PType};
    {final, DType} ->
      build_type(Module, DType, Address);
    undef ->
      {error, undefined}
  end.
%% end.

%% build_builtin_type(DType, Address) ->
%%     case DType of
%%      "void *" -> {raw_deref(Address), "undef"};
%%      "char *" -> cstr_to_list({Address, "nifty.char *"});
%%      _ -> build_type(nifty, DType, Address)
%%     end.

build_type(Module, Type, Address) ->
  Types = Module:get_types(),
  case dict:is_key(Type, Types) of
    true ->
      RType = nifty_types:resolve_type(Type, Types),
      {Kind, Def} =  dict:fetch(RType, Types),
      case Kind of
        userdef ->
          case Def of
            [{struct, Name}] ->
              Module:erlptr_to_record({Address, Name});
            [{union, Name}] ->
              Module:erlptr_to_urecord({Address, Name});
            _ ->
              {error, undefined}
          end;
        base ->
          case Def of
            ["char", Sign, _] ->
              int_deref(Address, 1, Sign);
            ["int", Sign, L] ->
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
            ["float", _, _] ->
              float_deref(Address);
            ["double", _, _] ->
              double_deref(Address);
            _ ->
              {error, unknown_builtin_type}
          end;
        _ ->
          {error, unknown_type}
      end;
    false ->
      {error, unknown_type}
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

%% @doc Allocates the specified amount of bytes and returns a pointer
%% to the allocated memory
-spec malloc(non_neg_integer()) -> ptr().
malloc(Size) ->
  mem_alloc(Size).

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

%% @doc Converts an erlang string into a 0 terminated C string and
%% returns a nifty pointer to it
-spec list_to_cstr(string()) -> ptr().
list_to_cstr(_) ->
  erlang:nif_error(nif_library_not_loaded).

%% @doc Converts a nifty pointer to a 0 terminated C string into a
%% erlang string.
-spec cstr_to_list(ptr()) -> string().
cstr_to_list(_) ->
  erlang:nif_error(nif_library_not_loaded).

%% @doc size of a base type, no error handling
-spec size_of(nonempty_string()) -> integer() | undef.
size_of(Type) ->
  Types = get_types(),
  case dict:is_key(Type, Types) of
    true ->
      %% builtin
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
      end;
    false ->
      %% full referenced
      case string:tokens(Type, ".") of
        ["nifty", TypeName] ->
          %% builtin
          size_of(TypeName);
        [ModuleName, TypeName] ->
          Mod = list_to_atom(ModuleName),
          case {module, Mod} =:= code:ensure_loaded(Mod) andalso
            proplists:is_defined(size_of, Mod:module_info(exports)) of
            true ->
              Mod:size_of(TypeName);
            false ->
              undef
          end;
        _ ->
          undef
      end
  end.

%% @doc Returns the integer value associated with an enum alias
-spec enum_value(atom(), nonempty_string() | atom()) -> integer() | undef.
enum_value(Module, Value) when is_atom(Value) ->
  enum_value(Module, atom_to_list(Value));
enum_value(Module, Value) ->
  case {module, Module} =:= code:ensure_loaded(Module) andalso
    proplists:is_defined(get_enum_aliases, Module:module_info(exports)) of
    true ->
      case proplists:lookup(Value, Module:get_enum_aliases()) of
        {Value, IntValue} -> IntValue;
        _ -> undef
      end;
    false ->
      undef
  end.


%% @doc Returns a pointer to a memory area that is the size of a pointer
-spec pointer() -> ptr().
pointer() ->
  {_, Size} = proplists:get_value("arch", nifty:get_config()),
  mem_alloc(Size).

referred_type(Type) ->
  case lists:last(Type) of
    $* -> Type++"*";
    _ -> Type++" *"
  end.

%% @doc Returns a pointer to the specified <code>Type</code>. This
%% function allocates memory of <b>sizeof(</b><code>Type</code><b>)</b>
-spec pointer(nonempty_string()) -> ptr() | undef.
pointer(Type) ->
  case size_of(Type) of
    undef -> undef;
    S -> as_type(mem_alloc(S), referred_type(Type))
  end.

%% @doc Returns a pointer to the given pointer
-spec pointer_of(ptr()) -> ptr() | undef.
pointer_of({_, Type} = Ptr) ->
  pointer_of(Ptr, Type).

%% @doc Returns a pointer to the <code>Value</code> with the type
%% <code>Type</code>
-spec pointer_of(term(), string()) -> ptr() | undef.
pointer_of(Value, Type) ->
  case string:right(Type, 1) of
    "*" ->
      %% pointer
      {Addr, VType} = Value,
      case VType =:= Type of
        true ->
          {_, Size} = proplists:get_value("arch", nifty:get_config()),
          {NAddr, _} = int_constr(Addr, Size),
          {NAddr, Type++"*"};
        false ->
          undef
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
            undef ->
              %% no base type, try the module
              %% resolve type and try again
              Module = list_to_atom(ModuleName),
              Types = Module:get_types(),
              case nifty_types:resolve_type(T, Types) of
                undef ->
                  %% can (right now) only be a struct or a union
                  structured_pointer_of(Module, Value);
                ResT ->
                  case builtin_pointer_of(Value, ResT) of
                    undef ->
                      %% can (right now) only be a struct
                      structured_pointer_of(Module, Value);
                    Ptr ->
                      Ptr
                  end
              end;
            Ptr ->
              Ptr
          end
      end
  end.

structured_pointer_of(Module, Value) ->
  Tag = atom_to_list(element(1, Value)),
  case string:str(Tag, "struct") of
    1 -> Module:record_to_erlptr(Value);
    _ -> case string:str(Tag, "union") of
           1 -> Module:urecord_to_erlptr(Value);
           _ -> undef
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
               undef ->
                 undef;
               Size ->
                 case is_integer(Value) of
                   true ->
                     as_type(int_constr(Value, Size), "nifty."++Type++" *");
                   false ->
                     undef
                 end
             end
      end;
    false ->
      undef
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

%% @doc Writes the <code>Data</code> to the memory area pointed to by
%% <code>Ptr</code> and returns a the pointer; the list elements are
%% interpreted as byte values
-spec mem_write(ptr(), binary() | list()) -> ptr().
mem_write({Addr, _} = Ptr, Data) ->
  {Addr, _} = case is_binary(Data) of
                true ->
                  mem_write_binary(Data, Ptr);
                false ->
                  mem_write_list(Data, Ptr)
              end,
  Ptr.

%% @doc Writes the <code>Data</code> to memory and returns a nifty
%% pointer to it; the list elements are interpreted as byte values
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

%% @doc Reads <code>X2</code> bytes from the pointer <code>X1</code>
%% and returns it as list
-spec mem_read(ptr(), integer()) -> list().
mem_read(_, _) ->
  erlang:nif_error(nif_library_not_loaded).

%% @doc Allocates <code>X1</code> bytes and returns a pointer to it
-spec mem_alloc(non_neg_integer()) -> ptr().
mem_alloc(_) ->
  erlang:nif_error(nif_library_not_loaded).

%% @doc Copies <code>Size</code> bytes from <code>Ptr1</code> to
%% <code>Ptr2</code>
-spec mem_copy(ptr(), ptr(), non_neg_integer()) -> ok.
mem_copy(_, _, _) ->
  erlang:nif_error(nif_library_not_loaded).

%% @doc Returns the platform specific configuration of nifty
-spec get_config() -> proplists:proplist().
get_config() ->
  erlang:nif_error(nif_library_not_loaded).

%% @doc Returns erlangs NIF environment
-spec get_env() -> {integer(), nonempty_string()}.
get_env() ->
  erlang:nif_error(nif_library_not_loaded).

%% @doc Casts a pointer to <code>Type</code>; returns <code>undef</code>
%% if the specified type is invalid
-spec as_type(ptr(), nonempty_string()) -> ptr() | undef.
as_type({Address, _} = Ptr, Type) ->
  BaseType = case string:tokens(Type, "*") of
               [T] ->
                 string:strip(T);
               _ ->
                 []
             end,
  Types = get_types(),
  case dict:is_key(BaseType, Types) of
    true ->
      {Address, "nifty."++Type};
    false ->
      case string:tokens(Type, ".") of
        ["nifty", TypeName] ->
          %% builtin type
          as_type(Ptr, TypeName);
        [ModuleName, TypeName] ->
          Mod = list_to_atom(ModuleName),
          case {module, Mod} =:= code:ensure_loaded(Mod) andalso
            proplists:is_defined(get_types, Mod:module_info(exports)) of
            true ->
              %% resolve and build but we are looking for the basetype
              %% if the base type is defined or basetype * we are allowing
              %% casting
              [RBUType] = string:tokens(TypeName, "*"),
              RBType = string:strip(RBUType),
              case nifty_types:resolve_type(RBType, Mod:get_types()) of
                undef ->
                  case nifty_types:resolve_type(RBType++" *", Mod:get_types()) of
                    undef ->
                      %% unknown type
                      undef;
                    _ ->
                      %% pointer to incomplete type
                      {Address, Type}
                  end;
                _ ->
                  %% pointer to complete type
                  {Address, Type}
              end;
            _ ->
              %% module part of the type is not a nifty module
              undef
          end;
        _ ->
          %% malformed type
          undef
      end
  end.

%% @doc Allocates an array with <code>Size</code> elements of type
%% <code>Type</code>
-spec array_new(nonempty_string(), non_neg_integer()) -> ptr().
array_new(Type, Size) ->
  {Addr, _} = malloc(size_of(Type)*Size),
  {Addr, referred_type(Type)}.

array_element_type({_, Type}) ->
  string:strip(lists:droplast(Type)).

%% @doc returns a pointer to the element at position
%% <code>Index</code> of the array
-spec array_ith(ptr(), integer()) -> ptr().
array_ith({Addr, Type} = Array, Index) ->
  %% Type must be a pointer of the stored type
  Offset = size_of(array_element_type(Array)),
  {Addr + (Index * Offset), Type}.

%% @doc returns the element at position <code>Index</code> of the array
-spec array_element(ptr(), integer()) -> cvalue().
array_element(Array, Index) ->
  dereference(array_ith(Array, Index)).

%% @doc updates the element at position <code>Index</code> of the array
-spec array_set(ptr(), term(), integer()) -> ok.
array_set(Array, Value, Index) ->
  ElementPtr = array_ith(Array, Index),
  NewElement = pointer_of(Value, array_element_type(Array)),
  Size = size_of(array_element_type(Array)),
  mem_copy(NewElement, ElementPtr, Size),
  free(NewElement).

-spec array_to_list(ptr(), non_neg_integer()) -> [cvalue()].
array_to_list(Array, N) ->
  [array_element(Array, I) || I <- lists:seq(0,N)].

-spec list_to_array(list(), nonempty_string()) -> ptr().
list_to_array(List, Type) ->
  N = length(List),
  A = array_new(Type, N),
  _ = [array_set(A, E, I) || { I, E} <- lists:zip(lists:seq(0,N-1), List) ],
  A.
