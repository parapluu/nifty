#include <erl_nif.h>
#include <string.h>
#include <stdint.h>

#include <stdio.h>

#include "{{header|absname}}"

#if _WIN32 || _WIN64
	#if _WIN64
typedef unsigned __int64 uint64_t;
#define ENV64BIT
	#else
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

#ifdef ENV32BIT
typedef unsigned long ptr_t;
#define nifty_get_ptr(env, term, ip) enif_get_ulong((env), (term), (ip))
#define nifty_make_ptr(env, i) enif_make_ulong((env), (i))
#else /* ENV64BIT */
typedef uint64_t ptr_t;
#define nifty_get_ptr(env, term, ip) enif_get_uint64((env), (term), (ip))
#define nifty_make_ptr(env, i) enif_make_uint64((env), (i))
#endif

{% if config|config_schedule_dirty %}
#ifndef ERL_NIF_DIRTY_SCHEDULER_SUPPORT
#warning "Dirty Schedulers are not supported, compiling without..."
#endif
{% endif %}

/*
 * forward declarations
 */
{% with prototypes=1 %}
{% include "structures.tpl" %}
{% endwith %}

/*
 * Structs
 */
{% include "structures.tpl" %}

/*
 * Build Function Definitions
 */
{% include "function.tpl" %}

/*
 * Function definitions for ErLang
 */
static ErlNifFunc nif_functions[] = {
	{% with fn=symbols|fetch_keys %}{% for name in fn %}
	{"{{name}}", {{ symbols|fetch:name|length|add:-1 }}, _nifty_{{name}}, {% if config|config_schedule_dirty %} ERL_NIF_DIRTY_JOB_CPU_BOUND {% endif %}},
	{% endfor %}{% endwith %}
	{"erlptr_to_record", 1, erlptr_to_record},
	{"record_to_erlptr", 1, record_to_erlptr},
	{"erlptr_to_urecord", 1, erlptr_to_urecord},
	{"urecord_to_erlptr", 1, urecord_to_erlptr},
	{"new", 1, new_type_object},
	{"size_of", 1, size_of}
	};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

ERL_NIF_INIT({{module}}, nif_functions, NULL, NULL, upgrade, NULL);
