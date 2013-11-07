{% for name, data in functions %}
static ERL_NIF_TERM
erl2c_{{name}}(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	/* make flag dependent */
	unsigned l;
	{% for rettype, args in data %}
	{{rettype}} c_retval;
	ERL_NIF_TERM retval;
	
	{% for aname, atype in args %}
	ERL_NIF_TERM arg_{{aname}} = argv[{{forloop.counter0}}];
	{{atype}} c_arg_{{forloop.counter0}};
	{% endfor %}{% endfor %}

/*
 * build arguments
 */
{% for rettype, args in data %}{% for rawname, type in args %}
{% include "from_erl/build.tpl"  with cname="c_arg_"|add:forloop.counter0 erlname="argv["|add:forloop.counter0|add:"]" %}
{% endfor %}{% endfor %}

/*
 * call the c function
 */
	c_retval = {{name}}(
{% for rettype, args in data %}{% for aname, atype in args %}
		c_arg_{{forloop.counter0}}{% if not forloop.parentloop.last %},{%endif%}
{% endfor %}{% endfor %}
	);

/*
 * build return value
 */
{% for type, args in data %}
{% include "to_erl/build.tpl" with cname="c_retval"  erlname="retval" %}
{% endfor %}
/*
 * cleanup
 */
{% for rettype, args in data %}{% for rawname, type in args %}
{% include "cleanup/build.tpl" with cname="c_arg_"|add:forloop.counter0 %}
{% endfor %}{% endfor %}
/*
 * return value
 */
	return retval;
/*
 * error handling
 */
}

{% endfor %}
