{
	application,
	{{module}},
	[
		{vsn,
			"1.0"},
		{description,
			"Nifty interface of {{module}}"},
		{modules, [
			{{module}},
			{{module}}_remote]},
		{applications,[kernel,stdlib,compile]}
	]
}.
