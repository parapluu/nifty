-module(nifty_compile_templates).
-export([main/1]).

main(_) ->
    ok = io:format("Compiling Templates~n"),
    Options = [{out_dir, "ebin"}, 
	       {force_recompile, true}, 
	       {custom_tags_modules, [nifty_tags]}, 
	       {custom_filters_modules, [nifty_filters]}, 
	       {compiler_options, [debug_info]}],
    {ok, nifty_c_template} = erlydtl:compile("templates/cmodule.tpl", nifty_c_template, Options),
    {ok, nifty_erl_template} = erlydtl:compile("templates/emodule.tpl", nifty_erl_template, Options),
    {ok, nifty_save_erl_template} = erlydtl:compile("templates/save_emodule.tpl", nifty_save_erl_template, Options),
    {ok, nifty_hrl_template} = erlydtl:compile("templates/hrlmodule.tpl", nifty_hrl_template, Options),
    {ok, nifty_app_template} = erlydtl:compile("templates/app.tpl", nifty_app_template, Options),
    {ok, nifty_config_template} = erlydtl:compile("templates/config.tpl", nifty_config_template, Options).

