{% ifequal rettype "int" %}
	retval = enif_make_int(env, c_retval);
{% endifequal %}
{% ifequal rettype "unsigned int" %}
	retval = enif_make_uint(env, c_retval);
{% endifequal %}