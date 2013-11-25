{% with N=argument|getNth:2 %}
	{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}

{% if phase=="prepare" %}
	{% if argument|is_argument %}
	uint64_t {{carg}};
	{% else %}
	uint64_t retval;
	ERL_NIF_TERM retval;
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	err = enif_get_uint64(env, {{erlarg}}, &{{carg}});
{% endif %}

{% if phase=="argument" %}
{% if argument|is_argument %}
({{type}}){{carg}}
{% else %}
(uint64_t)
{% endif %}
{% endif %}

{% if phase=="to_erl" %}
	retval = enif_make_uint64(env, c_retval);
{% endif %}

	{% endwith %}
{% endwith %}