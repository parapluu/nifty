{% if phase=="prepare" %}
	{% if argument|is_return %}
	ERL_NIF_TERM retval;
	{% endif %}
{% endif %}

{% if phase=="to_erl"%}
	{{erlarg}} = enif_make_atom(env, "ok");
{% endif %}
