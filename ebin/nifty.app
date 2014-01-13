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
			type_table]},
		{applications,[kernel,stdlib,compile]}
	]
}.
