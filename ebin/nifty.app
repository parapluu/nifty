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
			nifty_tags,
			nifty_rebar,
			nifty_compiler,
			nifty_utils,
			nifty_typetable]},
		{applications,[kernel,stdlib,compile]}
	]
}.
