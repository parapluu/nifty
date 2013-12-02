{% with N=argument|getNth:2 %}
	{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}


{% if phase=="prepare" %}
	{% if argument|is_argument %}
	{{type}} {{carg}};
	{% if typedef|getNth:3=="short" %}
		{% if typedef|getNth:2=="unsigned" %}
	unsigned int {{carg}}_helper;
		{% else %}
	int {{carg}}_helper;
		{% endif %}
	{% endif %}
	{% else %}
	{{type}} c_retval;
	ERL_NIF_TERM retval;
	{% endif %}
{% endif %}



	{% endwith %}
{% endwith %}
