{% if phase=="prepare" %}
	{% if argument|is_argument or argument|is_field %}
	{{type}} {{carg}};
		{% if typedef|getNth:3=="short" %}
			{% if typedef|getNth:2=="unsigned" %}
	unsigned int {{carg}}_helper;
			{% else %}
	int {{carg}}_helper;
			{% endif %}
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
	err = 
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="short" %}enif_get_uint{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="short" %}enif_get_int{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="none" %}enif_get_uint{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="none" %}enif_get_int{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="long" %}enif_get_ulong{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="long" %}enif_get_long{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="longlong" %}enif_get_uint64{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="longlong" %}enif_get_int64{% endif %}
	(env, {{erlarg}}, &{% if typedef|getNth:3=="short" %}{{carg}}_helper{% else %}{{carg}}{% endif %});
	{% if typedef|getNth:3=="short" %}
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
	{{erlarg}} = 
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="short" %}({{type}})enif_make_uint{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="short" %}({{type}})enif_make_int{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="none" %}enif_make_uint{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="none" %}enif_make_int{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="long" %}enif_make_ulong{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="long" %}enif_make_long{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="longlong" %}enif_make_uint64{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="longlong" %}enif_make_int64{% endif %}(env, {{carg}});
{% endif %}

{# no cleanup phase #}
