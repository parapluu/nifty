{% if phase=="prepare" %}
	{% if argument|is_argument or argument|is_field %}
	{{type}} {{carg}};
		{% if typedef|getNth:1=="float" %}
	double {{carg}}_helper;
		{% endif %}
	{% endif %}

	{% if argument|is_return %}
	{{type}} c_retval;
	ERL_NIF_TERM retval;
	{% endif %}

	{% if argument|is_field %}
	ERL_NIF_TERM {{erlarg}};
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	err = enif_get_double(env, {{erlarg}}, &{{carg}});
	{% if typedef|getNth:1=="float" %}
	{{carg}}=({{type}}){{carg}}_helper;
	{% endif %}
{% endif %}

{% if phase=="argument" %}
{% if argument|is_argument %}
({{raw_type}}){{carg}}
{% else %}
({{type}})
{% endif %}
{% endif %}

{% if phase=="to_erl"%}
	{{erlarg}} = {{type}}enif_make_double(env, {{carg}});
{% endif %}

{# no cleanup phase #}
