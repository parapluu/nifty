{
	application,
	nifty,
	[
		{description,
			"NIF interface generator"}, 
		{vsn,
			"0.1"},
		{modules, [
			nifty, 
			nifty_clangparse,
			nifty_compiler,
			nifty_filters,
			nifty_rebar,
			nifty_remote,
			nifty_remotecall,
			nifty_tags,
			nifty_typetable,
			nifty_utils]},
		{applications,[kernel,stdlib,compile]}
	]
}.
