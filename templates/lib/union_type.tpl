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
	{{raw_type}}* {{N}}_union;
		{% endif %}
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	{% if argument|is_field %}
	{{N}}_ptr  = urecord_to_erlptr_{{types|fetch:type|getNth:2|getNth:1|getNth:2}}(env, {{erlarg}});
	err = enif_get_tuple(env, {{N}}_ptr, &{{N}}_ar, (const ERL_NIF_TERM**)(&{{N}}_tpl));
	if (err) {
		err = nifty_get_ptr(env, {{N}}_tpl[0], (ptr_t*)(&{{N}}_union));
	}
	{{carg}} = *{{N}}_union;
	{% else %}
	{{carg}}_ptr  = urecord_to_erlptr_{{types|fetch:type|getNth:2|getNth:1|getNth:2}}(env, {{erlarg}});
	err = enif_get_tuple(env, {{carg}}_ptr, &{{carg}}_ar, (const ERL_NIF_TERM**)(&{{carg}}_tpl));
	if (err) {
		err = nifty_get_ptr(env, {{carg}}_tpl[0], (ptr_t*)(&{{carg}}));
	}
	{% endif %}
{% endif %}

{% if phase=="argument" %}
	{% if argument|is_argument %}
({{raw_type|discard_restrict}})(*{{carg}})
	{% endif %}
{% endif %}

{% if phase=="to_erl"%}
{{erlarg}} = ptr_to_urecord_{{types|fetch:type|getNth:2|getNth:1|getNth:2}}(env, (ptr_t)(&{{carg}}));
{% endif %}

{% if phase=="cleanup"%}
	enif_free({{carg}});
{% endif %}
