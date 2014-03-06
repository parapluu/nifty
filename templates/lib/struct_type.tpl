{% if phase=="prepare" %}
	{% if argument|is_argument %}
	{{raw_type}}* {{carg}};
	ERL_NIF_TERM {{carg}}_ptr;
	ERL_NIF_TERM *{{carg}}_tpl;
	int {{carg}}_ar;
	{% endif %}
	{% if argument|is_return %}
	{{raw_type}} c_retval;
	ERL_NIF_TERM retval;
	{% endif %}
	{% if argument|is_field %}
		{% if record=="to_record" %}
	ERL_NIF_TERM {{erlarg}};
		{% endif %}
		{% if record=="to_ptr" %}
	ERL_NIF_TERM *{{N}}_tpl;
	ERL_NIF_TERM {{N}}_ptr;
	int {{N}}_ar;
		{% endif %}
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	{% if argument|is_field %}
	{{N}}_ptr  = record_to_erlptr_{{types|fetch:type|getNth:2|getNth:1}}(env, {{erlarg}});
	err = enif_get_tuple(env, {{N}}_ptr, &{{N}}_ar, (const ERL_NIF_TERM**)(&{{N}}_tpl));
	if (err) {
		err = enif_get_uint64(env, {{N}}_tpl[0], (uint64_t*)(&{{carg}}));
	}
	{% else %}
	{{carg}}_ptr  = record_to_erlptr_{{types|fetch:type|getNth:2|getNth:1}}(env, {{erlarg}});
	err = enif_get_tuple(env, {{carg}}_ptr, &{{carg}}_ar, (const ERL_NIF_TERM**)(&{{carg}}_tpl));
	if (err) {
		err = enif_get_uint64(env, {{carg}}_tpl[0], (uint64_t*)(&{{carg}}));
	}
	{% endif %}
{% endif %}

{% if phase=="argument" %}
	{% if argument|is_argument %}
({{raw_type}})(*{{carg}})
	{% endif %}
{% endif %}

{% if phase=="to_erl"%}
{{erlarg}} = ptr_to_record_{{types|fetch:type|getNth:2|getNth:1}}(env, (uint64_t)(&{{carg}}));
{% endif %}

{% if phase=="cleanup"%}
	enif_free({{carg}});
{% endif %}
