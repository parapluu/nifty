{% if phase=="prepare" %}
	{% if argument|is_argument%}
	double {{carg}};
	{% endif %}

	{% if argument|is_return %}
	double c_retval;
	ERL_NIF_TERM retval;
	{% endif %}

	{% if argument|is_field %}
		{% if record=="to_record" %}
	ERL_NIF_TERM {{erlarg}};
		{% endif %}
		{% if record=="to_ptr" %}
	double helper{{N}};
		{% endif %}
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	{% if argument|is_field and typedef|getNth:1=="float" %}
	err = enif_get_double(env, {{erlarg}}, &helper{{N}});
	{{carg}} = (float)helper{{N}};
	{% else %}
	err = enif_get_double(env, {{erlarg}}, &{{carg}});
	{% endif %}
{% endif %}

{% if phase=="argument" %}
{% if argument|is_argument %}
({{raw_type|discard_restrict}}){{carg}}
{% else %}
({{type}})
{% endif %}
{% endif %}

{% if phase=="to_erl"%}
	{{erlarg}} = enif_make_double(env, {{carg}});
{% endif %}


{# no cleanup phase #}
