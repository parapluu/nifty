{% for name, data in functions %}
static ERL_NIF_TERM
erl2c_{{name}}(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	{% for rettype, args in data %}
	{{rettype}} c_retval;
	ERL_NIF_TERM retval;
	
	{% for aname, atype in args %}
	ERL_NIF_TERM arg_{{aname}} = argv[{{forloop.counter0}}];
	{{atype}} c_arg_{{aname}};
	{% endfor %}{% endfor %}

{% include "build_arg/build.tpl" %}

{#
 # call the c function
 #}
	c_retval = {{name}}(
{% for rettype, args in data %}{% for aname, atype in args %}
		c_arg_{{aname}}{% if not forloop.parentloop.last %},{%endif%}
{% endfor %}{% endfor %}
	);

{% include "build_retval/build.tpl" %}
	return retval;
}
{% endfor %}
