{% with type_tuple=types|fetch:type %}
	{% with kind=type_tuple|getNth:1 typedef=type_tuple|getNth:2 %}
		{% if "int" == typedef|getNth:1 or "char" == typedef|getNth:1%}
			{% include "lib/int_type.tpl" %}
		{% else %}{% if "float" == typedef|getNth:1 or "double" == typedef|getNth:1 %}
			{% include "lib/float_type.tpl" %}
		{% else %}{% if "userdef" == kind and not ("*" in typedef|getNth:1) and not ("[" in typedef|getNth:1) and ("struct" in type) %}
			{% include "lib/struct_type.tpl" %}
		{% else %}{% if "union" == kind and not ("*" in typedef|getNth:1) %}
			// TODO Union
		{% else %}{% if "void" == type%}
			{% include "lib/void.tpl" %}
		{% else %}{% if typedef|getNth:1|is_array %}
			{% include "lib/array_type.tpl" %}
		{% else %}
			{% include "lib/pointer_type.tpl" %}
		{% endif %}{% endif %}{% endif %}{% endif %}{% endif %}{% endif %}
	{% endwith %}
{% endwith %}
