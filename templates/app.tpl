{
	application,
	{{module}},
	[
		{description,
			"Nifty interface of {{module}}"},
		{modules, [
			{{module}},
			{{module}}_remote]},
		{applications,[kernel,stdlib,compile]}
	]
}.
