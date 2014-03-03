-module(nifty_compiler).
-export([
	 render/4,
	 store_files/4,
	 compile_module/3,
	 compile/3
	]).

render(InterfaceFile, Module, CFlags, Options) ->
    io:format("generating ~s -> ~s ~s ~n", [InterfaceFile, Module++"_nif.c", Module++".erl"]),
    %% c parse stuff
    PathToH = InterfaceFile,
    case clang_parse:parse([PathToH|CFlags]) of
	{fail, _} -> 
	    fail;
	{Token, _} -> 
	    {Functions, Typedefs, Structs} = clang_parse:build_vars(Token),
	    {Types, Symbols} = type_table:build({Functions, Typedefs, Structs}),
	    RenderVars = [
			  {"functions", Functions},  % ?
			  {"structs", Structs},      % ?
			  {"typedefs", Typedefs},    % ? 
			  {"module", Module},
			  {"header", InterfaceFile},
			  {"config", Options},
			  {"types", Types},
			  {"symbols", Symbols},
			  {"none", none}
			 ],
	    {ok, COutput} = nifty_c_template:render(RenderVars),
	    {ok, ErlOutput} = nifty_erl_template:render(RenderVars),
	    {ok, AppOutput} = nifty_app_template:render(RenderVars),
	    {ok, ConfigOutput} = nifty_config_template:render(RenderVars),
	    {ErlOutput, COutput, AppOutput, ConfigOutput}
    end.

store_files(InterfaceFile, Module, Options, RenderOutput) ->
    {ok, Path} = file:get_cwd(),
    store_files(InterfaceFile, Module, Options, RenderOutput, Path).

store_files(_, Module, _, RenderOutput, Path) ->
    ok = case file:make_dir(filename:join([Path,Module])) of
	     ok -> ok;
	     {error,eexist} -> ok;
	     _ -> fail
	 end,
    ok = case file:make_dir(filename:join([Path,Module, "src"])) of
	     ok -> ok;
	     {error,eexist} -> ok;
	     _ -> fail
	 end,
    ok = case file:make_dir(filename:join([Path,Module, "c_src"])) of
	     ok -> ok;
	     {error,eexist} -> ok;
	     _ -> fail
	 end,
    ok = case file:make_dir(filename:join([Path,Module, "ebin"])) of
	     ok -> ok;
	     {error,eexist} -> ok;
	     _ -> fail
	 end,
    {ErlOutput, COutput, AppOutput, ConfigOutput} = RenderOutput,
    ok = fwrite_render(Path, Module, "src", Module++".erl", ErlOutput),
    ok = fwrite_render(Path, Module, "c_src", Module++"_nif.c", COutput),
    ok = fwrite_render(Path, Module, "ebin", Module++".app", AppOutput),
    ok = fwrite_render(Path, Module, ".", "rebar.config", ConfigOutput).

fwrite_render(Path, Module, Dir, FileName, Template) ->
    file:write_file(filename:join([Path, Module, Dir, FileName]), [Template]).

compile_module(_, Module, _) ->
    {ok, Path} = file:get_cwd(),
    ok = file:set_cwd(filename:join([Path, Module])),
    rebar_commands(["compile"]),
    file:set_cwd(Path).

rebar_commands(Commands) ->
    RawArgs = Commands,
    Args = nifty_rebar:parse_args(RawArgs),
    BaseConfig = nifty_rebar:init_config(Args),
    {BaseConfig1, Cmds} = nifty_rebar:save_options(BaseConfig, Args),
    nifty_rebar:run(BaseConfig1, Cmds).


compile(InterfaceFile, Module, Options) ->
    ModuleName = erlang:atom_to_list(Module),
    UCO = update_compile_options(InterfaceFile, ModuleName, Options),
    Env = build_env(ModuleName, UCO),
    CFlags = string:tokens(proplists:get_value("CFLAGS", Env, ""), " "),
    case render(InterfaceFile, ModuleName, CFlags, UCO) of
	fail -> 
	    undefined;
	Output ->
	    case store_files(InterfaceFile, ModuleName, UCO, Output) of
		ok ->
		    compile_module(InterfaceFile, ModuleName, UCO), 
		    ok;
		_ -> 
		    undefined
	    end
    end,
    ModulePath = filename:absname(filename:join([ModuleName, "ebin"])),
    code:add_path(ModulePath),
    ok.


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
    rebar_port_compiler:setup_env({config, undefined, [{port_env, EnvAll}], undefined, undefined, undefined, dict:new()}).

get_spec_env(_, []) -> [];
get_spec_env(ModuleName, [S|T]) ->
    Lib = libname(ModuleName),
    case S of
	{_, Lib, _, Options} ->
	    case proplists:get_value(env, Options) of
		undefined -> [];
		Env -> Env
	    end;
	_ ->
	    get_spec_env(ModuleName, T)
    end.

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
      join_options([{env, [{"CFLAGS", "$CFLAGS -I"++filename:absname(filename:dirname(InterfaceFile))}]}], Options)
    }.

join_options(Proplist1, Proplist2) ->
    orddict:merge(
      fun(_,X,Y) -> X++Y end,
      orddict:from_list(Proplist1),
      orddict:from_list(Proplist2)).

abspath_sources(S) -> abspath_sources(S, []).

abspath_sources([], Acc) -> Acc;
abspath_sources([S|T], Acc) ->
    abspath_sources(T, [filename:absname(S)|Acc]).


update_port_spec(_,  _, [], Acc, true) -> 
    Acc;
update_port_spec(InterfaceFile,  ModuleName, [], Acc, false) -> 
    [module_spec(".*", [], [], InterfaceFile, ModuleName), Acc];
update_port_spec(InterfaceFile,  ModuleName, [Spec|T], Acc, Found) ->
    Shared = libname(ModuleName),
    case Spec of
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
