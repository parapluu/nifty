#include <erl_nif.h>

{#
 # Build Function Definitions
 #}
{% include "function.tpl" %}

{#
 # Function definitions for ErLang
 #}
static ErlNifFunc nif_funcs[] = {
{% for name, data in functions %}
{% for rettype, args in data %}
	{"{{name}}", {{args|length}}, erl2c_{{name}}},
{% endfor %}{% endfor %}
	}

ERL_NIF_INIT({{module}}, nif_funcs, NULL, NULL, upgrade, NULL);
