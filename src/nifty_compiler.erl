-module(nifty_compiler).
-export([
	rebar_commands/1
	]).

% TODO
%
% create:
% 
% 	module/src/
% 	module/src/module.erl
% 	module/c_src/
% 	module/c_src/module_nif.c
% 	module/ebin/module.app

rebar_commands(Commands) ->
	RawArgs = Commands,
	Args = nifty_rebar:parse_args(RawArgs),
	BaseConfig = nifty_rebar:init_config(Args),
	{BaseConfig1, Cmds} = nifty_rebar:save_options(BaseConfig, Args),
	nifty_rebar:run(BaseConfig1, Cmds).
