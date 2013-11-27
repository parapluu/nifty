{% with N=argument|getNth:2 %}
	{% with carg="carg_"|add:N erlarg="erlarg_"|add:N %}


{% if phase=="prepare" %}
	{% if argument|is_argument %}
	{{type}} {{carg}};
		{% if typedef|getNth:1=="float" %}
	double {{carg}}_helper;
		{% endif %}
	{% endif %}
{% else %}
	{{type}} c_retval;
	ERL_NIF_TERM retval;
{% endif %}

{% if phase=="to_c" %}
	err = enif_get_double(env, {{erlarg}}, &{{carg}});
	{% if typedef|getNth:1=="float" %}
	{{carg}}=({{type}}){{carg}}_helper;
	{% endif %}
{% endif %}

{% if phase=="argument" %}
{% if argument|is_argument %}
({{type}}){{carg}}
{% else %}
({{type}})
{% endif %}
{% endif %}

{% if phase=="to_erl"%}
	retval = {{type}}enif_make_double(env, c_retval);
{% endif %}

{# no cleanup phase #}

	{% endwith %}
{% endwith %}
