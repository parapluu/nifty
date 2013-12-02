{% with N=argument|getNth:2 %}
	{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}

{% if phase=="prepare" %}
	{% if argument|is_argument %}
	ERL_NIF_TERM *tpl{{N}};
	int arity{{N}};
	uint64_t {{carg}};
	{% else %}
	uint64_t c_retval;
	ERL_NIF_TERM retval;
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
({{type}}){{carg}}
{% else %}
(uint64_t)
{% endif %}
{% endif %}

{% if phase=="to_erl" %}
	retval = enif_make_tuple3(
		env,
		enif_make_uint64(env, c_retval),
		enif_make_atom(env, "{{module}}"),
		enif_make_string(env, "{{type}}", ERL_NIF_LATIN1));
{% endif %}

	{% endwith %}
{% endwith %}
