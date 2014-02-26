{% if phase=="prepare" %}
	{% if argument|is_argument %}
	{{type|array_get_base_type}} {{carg}};
	{% endif %}
	{% if argument|is_return %}
	{{type}} c_retval; 
	ERL_NIF_TERM retval;
	{% endif %}
	{% if argument|is_field %}
		{% if record=="to_record" %}
	ERL_NIF_TERM {{erlarg}};
		{% endif %}
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	err = lst_to_array_{{typedef|array_name}}(env,
	                                          {{erlarg}},
	                                          ({{type|array_get_base_type}}*)&{{carg}},
	                                          {{typedef|array_length}});
{% endif %}

{% if phase=="argument" %}
{% if argument|is_argument %}
({{type|array_get_base_type}}){{carg}}
{% else %}
{% endif %}
{% endif %}

{% if phase=="to_erl" %}
	{{erlarg}} = array_to_lst_{{typedef|array_name}}(env, 
	                                                 {{carg}},
	                                                 {{typedef|array_length}});
{% endif %}
