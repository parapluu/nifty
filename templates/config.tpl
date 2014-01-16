{port_specs, [
	{".*", "priv/{{module}}_nif.so", ["c_src/{{module}}_nif.c"], [{env, [{"CFLAGS", "$CFLAGS -I{{header|raw_path|absname}}"}]}]}
	]}.
