-module(nifty_compiler).
-export([
	render/3,
	store_files/4
	]).

render(InterfaceFile, Module, CompileOptions) ->
    io:format("processing ~s -> ~s ~s ~n", [InterfaceFile, Module++"_nif.c", Module++".erl"]),
    %% c parse stuff
    PathToH = InterfaceFile,
    case clang_parse:parse([PathToH|CompileOptions]) of
	{fail, _} -> fail;
	{Token, _} -> 
	    {Functions, Typedefs, Structs} = clang_parse:build_vars(Token),
	    {Types, Symbols} = type_table:build({Functions, Typedefs, Structs}),
	    %% template stuff
		CTemplate = erlang:list_to_atom("nifty_c_template"),
		ErlTemplate = erlang:list_to_atom("nifty_erl_template"),
		AppTemplate = erlang:list_to_atom("nifty_app_template"),
		ConfigTemplate = erlang:list_to_atom("nifty_config_template"),
	    RenderVars = [
			  {"functions", Functions},  % ?
			  {"structs", Structs},      % ?
			  {"typedefs", Typedefs},    % ? 
			  {"module", Module},
			  {"header", InterfaceFile},
			  {"types", Types},
			  {"symbols", Symbols},
			  {"none", none}
			 ],
	    {ok, COutput} = CTemplate:render(RenderVars),
	    {ok, ErlOutput} = ErlTemplate:render(RenderVars),
		{ok, AppOutput} = AppTemplate:render(RenderVars),
	    {ok, ConfigOutput} = ConfigTemplate:render(RenderVars),
	    io:format("done~n"),
	    {ErlOutput, COutput, AppOutput, ConfigOutput}
    end.

store_files(InterfaceFile, Module, CompileOptions, RenderOutput) ->
	{ok, Path} = file:get_cwd(),
	store_files(InterfaceFile, Module, CompileOptions, RenderOutput, Path).

store_files(InterfaceFile, Module, CompileOptions, RenderOutput, Path) ->
	ok = file:make_dir(filename:join([Path,Module])),
	ok = file:make_dir(filename:join([Path,Module, "src"])),
	ok = file:make_dir(filename:join([Path,Module, "c_src"])),
	ok = file:make_dir(filename:join([Path,Module, "ebin"])),
	{ErlOutput, COutput, AppOutput, ConfigOutput} = RenderOutput,
	ok = file:write_file(filename:join([Path,Module, "src", Module++".erl"]), [ErlOutput]),
	ok = file:write_file(filename:join([Path,Module, "c_src", Module++"_nif.c"]), [COutput]),
	ok = file:write_file(filename:join([Path,Module, "ebin", Module++".app"]), [AppOutput]),
	file:write_file(filename:join([Path,Module, "rebar.config"]), [ConfigOutput]).



% compile(Module, ReBarOptions) ->
% 	

% % TODO
% %
% % create:
% % 
% % 	module/src/
% % 	module/src/module.erl
% % 	module/c_src/
% % 	module/c_src/module_nif.c
% % 	module/ebin/module.app
% 
% rebar_commands(Commands) ->
% 	RawArgs = Commands,
% 	Args = nifty_rebar:parse_args(RawArgs),
% 	BaseConfig = nifty_rebar:init_config(Args),
% 	{BaseConfig1, Cmds} = nifty_rebar:save_options(BaseConfig, Args),
% 	nifty_rebar:run(BaseConfig1, Cmds).

% build_compiler_options(CompileOptions) ->
% 	
