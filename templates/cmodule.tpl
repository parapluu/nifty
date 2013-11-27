#include <erl_nif.h>
#include <string.h>
#include <stdint.h>

#include <stdio.h>

#include <{{header}}>

#if _WIN32 || _WIN64
	#if _WIN64
typedef unsigned __int64 uint64_t;
		#define ENV64BIT
	#else
typedef unsigned __int32 uint32_t;
		#define ENV32BIT
	#endif
#else // clang gcc
#include <stdint.h>
	#if __x86_64__
		#define ENV64BIT
	#else
		#define ENV32BIT
	#endif
#endif 


/*
 * Build Function Definitions
 */
{% include "function.tpl" %}


/*
 * conversion functions
 */
{% include "lib/charp_conversion.tpl" %}
{% include "lib/pointer_arith.tpl" %}

/*
 * Function definitions for ErLang
 */
static ErlNifFunc nif_funcs[] = {
	{% with fn=functions|fetch_keys %}{% for name in fn %}
	{"{{name}}", {{ functions|fetch:name|getNth:2|length }}, erl2c_{{name}}},
	{% endfor %}{% endwith %}
	{"free_ptr", 1, free_pointer},
	{"list_to_cstr", 1, list_to_cstr},
	{"cstr_to_list", 1, cstr_to_list}
	};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

ERL_NIF_INIT({{module}}, nif_funcs, NULL, NULL, upgrade, NULL);
