{% with fn=functions|fetch_keys %}{% for name in fn %}

static ERL_NIF_TERM
erl2c_{{name}}(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_argument %}
				{% with  N=argument|getNth:2 %}
					{% with carg="carg_"|add:N erlarg="erlarg_"|add:N %}
	{{argument|getNth:3}} {{carg}};
	ERL_NIF_TERM {{erlarg}};
					{% endwith %}
				{% endwith %}
			{%endif%}
		{%endfor%}
	{% endwith %}

/*
 * Variable transition inside of a function:
 *	prepare -- are additional variable definitions required -> e.a. short int translation
 *	to_c    -- translate the erlang values to c values
 *	to_erl  -- translate the return value and eventual output arguments to erlang types
 *	cleanup -- cleanup memory used by the function
 */


{% comment %}	/* make flag dependent */
	unsigned l;
	unsigned int conv_uint;
	int conv_int;
	ERL_NIF_TERM head, tail;
	{% for rettype, args in data %}
	{{rettype|norm_type}} c_retval;
	ERL_NIF_TERM retval;
	
	{% for aname, atype in args %}
	{{atype|norm_type}} c_arg_{{forloop.counter0}};
{% endfor %}{% endfor %}

/*
 * build arguments
 */
{% for rettype, args in data %}{% for rawname, atype in args %}
{% include "from_erl/build.tpl"  with cname="c_arg_"|add:forloop.counter0 erlname="argv["|add:forloop.counter0|add:"]" type=atype|discard_const %}
{% endfor %}{% endfor %}

/*
 * call the c function
 */
	c_retval = {{name}}(
{% for rettype, args in data %}{% for aname, atype in args %}
	c_arg_{{forloop.counter0}}{% if not forloop.last %},{%endif%}
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
{% for rettype, args in data %}{% for rawname, rtype in args %}
{% include "cleanup/build.tpl" with cname="c_arg_"|add:forloop.counter0 type=rtype|discard_const %}
{% endfor %}{% endfor %}
/*
 * return value
 */
	return retval;
/*
 * error handling
 */{% endcomment %}
 
	{% with aux=ets|lookup:"aux" %}
		{% if aux|has_key:"cleanup" %}
			{% with cleanups=aux|fetch:"cleanup" %}
				{% for cleanup in cleanups %}
	nif_free((void*){{cleanup}});
				{% endfor %}
			{% endwith %}
		{% endif %}
	{% endwith %}
}

{% endfor %}{% endwith %}
