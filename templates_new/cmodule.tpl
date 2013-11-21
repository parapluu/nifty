#include <erl_nif.h>

#include <{{header}}>


/*
 * Build Function Definitions
 */
{% include "function.tpl" %}
/*
 * Function definitions for ErLang
 */
static ErlNifFunc nif_funcs[] = {
	{% with fn=functions|fetch_keys %}{% for name in fn %}
	{"{{name}}", {{ functions|fetch:name|getNth:2|length }}, erl2c_{{name}}}{% if not forloop.last %},{%endif%}
	{% endfor %}{% endwith %}
	};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

ERL_NIF_INIT({{module}}, nif_funcs, NULL, NULL, upgrade, NULL);
