-module(nifty_compile_templates).
-export([main/1]).

main(_) ->
	io:format("Compiling Templates~n"),
	CTemplate = erlang:list_to_atom("nifty_c_template"),
	ErlTemplate = erlang:list_to_atom("nifty_erl_template"),
	AppTemplate = erlang:list_to_atom("nifty_app_template"),
	ConfigTemplate = erlang:list_to_atom("nifty_config_template"),
	ok = erlydtl:compile(
		"templates/cmodule.tpl",
		CTemplate,
		[{out_dir, "ebin"},
		{force_recompile, true},
		{custom_tags_modules, [nifty_tags]},
		{custom_filters_modules, [nifty_filters]}]),
	ok = erlydtl:compile(
		"templates/emodule.tpl",
		ErlTemplate,
		[{out_dir, "ebin"},
		{force_recompile, true},
		{custom_tags_modules, [nifty_tags]},
		{custom_filters_modules, [nifty_filters]}]),
	ok = erlydtl:compile(
		"templates/app.tpl",
		AppTemplate,
		[{out_dir, "ebin"},
		{force_recompile, true},
		{custom_tags_modules, [nifty_tags]},
		{custom_filters_modules, [nifty_filters]}]),
	ok = erlydtl:compile(
		"templates/config.tpl",
		ConfigTemplate,
		[{out_dir, "ebin"},
		{force_recompile, true},
		{custom_tags_modules, [nifty_tags]},
		{custom_filters_modules, [nifty_filters]}]).

