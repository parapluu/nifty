#include <erl_nif.h>

#include <{{header}}>

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
	};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

ERL_NIF_INIT({{module}}, nif_funcs, NULL, NULL, upgrade, NULL);
