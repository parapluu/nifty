-module(nifty_config).
-export([
	update_config/2
	]).

update_config(UserConfig, Config) ->
	NewPort_Spec = case proplists:get_value(port_spec, UserConfig) of
		undefined -> 
			proplists:get_value(port_spec, Config);
		UPortSpec ->	
			join_proplist(UPortSpec, proplists:get_value(port_spec, Config))
	end,
	NewPort_Env = case proplists:get_value(port_env, UserConfig) of
		undefined ->
			proplists:get_value(port_env, Config);
		UPortEnv ->
			join_proplist(UPortEnv, proplists:get_value(port_env, Config))
	end,
	[{port_spec, NewPort_Spec},{port_env, NewPort_Env}].


%% internal functions
join_proplist(Proplist1, Proplist2) ->
	orddict:merge(
		fun(_,X,Y) -> [X,Y] end,
		orddict:from_list(Proplist1),
		orddict:from_list(Proplist2)).

