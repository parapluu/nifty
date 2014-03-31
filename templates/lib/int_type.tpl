{% if phase=="prepare" %}
	{% if argument|is_argument %}
	{{type}} {{carg}};
		{% if typedef|getNth:3=="short" or typedef|getNth:1=="char" %}
			{% if typedef|getNth:2=="unsigned" %}
	unsigned int helper{{N}};
			{% else %}
	int helper{{N}};
			{% endif %}
		{% endif %}
	{% endif %}
	{% if argument|is_return %}
	{{type}} c_retval;
	ERL_NIF_TERM retval;
	{% endif %}
	{% if argument|is_field %}
		{% if record=="to_record" %}
	ERL_NIF_TERM {{erlarg}};
		{% endif %}
		{% if record=="to_ptr" and (typedef|getNth:1=="char" or typedef|getNth:3=="short") %}
			{% if typedef|getNth:2=="unsigned" %}
	unsigned int helper{{N}};
			{% else %}
	int helper{{N}};
			{% endif %}
		{% endif %}
	{% endif %}
{% endif %}

{% if phase=="to_c" %}
	err = 
	{% if typedef|getNth:2=="unsigned" and (typedef|getNth:3=="short" or typedef|getNth:1=="char") %}enif_get_uint{% endif %}
	{% if typedef|getNth:2=="signed" and (typedef|getNth:3=="short" or typedef|getNth:1=="char") %}enif_get_int{% endif %}
	{% if typedef|getNth:2=="unsigned" and (typedef|getNth:3=="none" and typedef|getNth:1=="int") %}enif_get_uint{% endif %}
	{% if typedef|getNth:2=="signed" and (typedef|getNth:3=="none" and typedef|getNth:1=="int") %}enif_get_int{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="long" %}enif_get_ulong{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="long" %}enif_get_long{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="longlong" %}enif_get_uint64{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="longlong" %}enif_get_int64{% endif %}
	(env, {{erlarg}}, &{% if typedef|getNth:3=="short" or typedef|getNth:1=="char" %}helper{{N}}{% else %}{{carg}}{% endif %});
	{% if typedef|getNth:3=="short" or typedef|getNth:1=="char" %}
	{{carg}}=({{type}})helper{{N}};
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
	{{erlarg}} = 
	{% if typedef|getNth:2=="unsigned" and (typedef|getNth:3=="short" or typedef|getNth:1=="char") %}enif_make_uint{% endif %}
	{% if typedef|getNth:2=="signed" and (typedef|getNth:3=="short" or typedef|getNth:1=="char") %}enif_make_int{% endif %}
	{% if typedef|getNth:2=="unsigned" and (typedef|getNth:3=="none" and typedef|getNth:1=="int") %}enif_make_uint{% endif %}
	{% if typedef|getNth:2=="signed" and (typedef|getNth:3=="none" and typedef|getNth:1=="int") %}enif_make_int{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="long" %}enif_make_ulong{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="long" %}enif_make_long{% endif %}
	{% if typedef|getNth:2=="unsigned" and typedef|getNth:3=="longlong" %}enif_make_uint64{% endif %}
	{% if typedef|getNth:2=="signed" and typedef|getNth:3=="longlong" %}enif_make_int64{% endif %}(env, {{carg}});
{% endif %}

{# no cleanup phase #}
