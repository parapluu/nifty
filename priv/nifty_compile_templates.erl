-module(nifty_compile_templates).
-export([main/1]).

main(_) ->
    io:format("Compiling Templates~n"),
    CTemplate = list_to_atom("nifty_c_template"),
    ErlTemplate = list_to_atom("nifty_erl_template"),
    AppTemplate = list_to_atom("nifty_app_template"),
    ConfigTemplate = list_to_atom("nifty_config_template"),
    Options = [{out_dir, "ebin"},
	       {force_recompile, true},
	       {custom_tags_modules, [nifty_tags]},
	       {custom_filters_modules, [nifty_filters]}, 
	       {compiler_options, [debug_info]}],
    {ok, _} = erlydtl:compile("templates/cmodule.tpl", CTemplate, Options),
    {ok, _} = erlydtl:compile("templates/emodule.tpl", ErlTemplate, Options),
    {ok, _} = erlydtl:compile("templates/app.tpl", AppTemplate, Options),
    {ok, _} = erlydtl:compile("templates/config.tpl", ConfigTemplate, Options).

