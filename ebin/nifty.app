{
	application,
	nifty,
	[
		{description,
			"NIF interface generator"}, 
		{vsn,
			"0.1"},
		{modules, [
			clang_parse, 
			nifty, 
			nifty_filters,
			nifty_config,
			nifty_tags,
			nifty_rebar,
			nifty_compiler,
			type_table]},
		{applications,[kernel,stdlib,compile]}
	]
}.
