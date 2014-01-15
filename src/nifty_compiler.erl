-module(nifty_compiler).
-export([
	render/3
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
	    CTemplate = erlang:list_to_atom("nifty_ctemplate"),
	    ETemplate = erlang:list_to_atom("nifty_etemplate"),
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
	    {ok, EOutput} = ETemplate:render(RenderVars),
	    io:format("done~n"),
	    {EOutput, COutput}
    end.

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
