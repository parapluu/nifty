{% if phase=="prepare" %}
	{% if argument|is_argument or argument|is_field %}
	ERL_NIF_TERM *tpl{{N}};
	int arity{{N}};
	uint64_t {{carg}};
	{% endif %}
	{% if argument|is_return %}
	uint64_t c_retval;
	ERL_NIF_TERM retval;
	{% endif %}
	{% if argument|is_field %}
	ERL_NIF_TERM {{erlarg}};
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	err = enif_get_tuple(env, {{erlarg}}, &arity{{N}}, (const ERL_NIF_TERM**)(&tpl{{N}}));
	if (err) {
		err = enif_get_uint64(env, tpl{{N}}[0], &{{carg}});
	}
{% endif %}

{% if phase=="argument" %}
{% if argument|is_argument %}
({{raw_type}}){{carg}}
{% else %}
(uint64_t)
{% endif %}
{% endif %}

{% if phase=="to_erl" %}
	{{erlarg}} = enif_make_tuple2(
		env,
		enif_make_uint64(env, {{carg}}),
		enif_make_string(env, "{{module}}.{{type}}", ERL_NIF_LATIN1));
{% endif %}
