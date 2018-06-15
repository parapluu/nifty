{% if phase=="prepare" %}
	{% if argument|is_argument %}
	ERL_NIF_TERM *tpl{{N}};
	int arity{{N}};
	{{type|array_type}} {{carg}}[{{types|fetch:type|array_size}}];
	ptr_t addr{{N}};
	{% endif %}
	{% if argument|is_return %}
	ptr_t c_retval; 
	ERL_NIF_TERM retval;
	{% endif %}
	{% if argument|is_field %}
		{% if record=="to_record" %}
	ERL_NIF_TERM {{erlarg}};
		{% endif %}
		{% if record=="to_ptr" %}
	ERL_NIF_TERM *tpl{{N}};
	int arity{{N}};
	ptr_t addr{{N}};
		{% endif %}
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	if (enif_compare({{erlarg}}, enif_make_atom(env, "null"))) {
		err = enif_get_tuple(env, {{erlarg}}, &arity{{N}}, (const ERL_NIF_TERM**)(&tpl{{N}}));
		if (err) {
			err = nifty_get_ptr(env, tpl{{N}}[0], &addr{{N}});
			memcpy(&{{carg}}, (const void *)addr{{N}}, sizeof({{carg}}));
		}
	}
{% endif %}

{% if phase=="argument" %}
{% if argument|is_argument %}
{{carg}}
{% else %}
(ptr_t)&
{% endif %}
{% endif %}

{% if phase=="to_erl" %}
	{{erlarg}} = enif_make_tuple2(
		env,
		nifty_make_ptr(env, (ptr_t)&{{carg}}),
		enif_make_string(env, "{{module}}.{{type}}", ERL_NIF_LATIN1));
{% endif %}
