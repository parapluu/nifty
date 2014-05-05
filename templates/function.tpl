{% with fn=symbols|fetch_keys %}{% for name in fn %}

static ERL_NIF_TERM
_nifty_{{name}}(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err=0;
{#
/*
 * Variable transition inside of a function:
 *	prepare -- are additional variable definitions required -> e.a. short int translation
 *	to_c    -- translate the erlang values to c values
 *	argument-- yield argument
 *	to_erl  -- translate the return value and eventual output arguments to erlang types
 *	cleanup -- cleanup memory used by the function
 */
#}


	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_argument %}
				{% with raw_type=argument|getNth:3 phase="prepare" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
			{% endif %}
			{% if argument|is_return %}
				{% with raw_type=argument|getNth:2 phase="prepare" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}

	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_argument %}
				{% with raw_type=argument|getNth:3 phase="to_c" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
	if (!err) {
		goto error;
	}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}
	
	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_return %}
				{% with raw_type=argument|getNth:2 phase="argument" %}
					{% with type=raw_type|resolved:types %}
						{% if not type=="void" %}
	c_retval =
						{% endif %}
					{% endwith %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}
	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_return %}
				{% with raw_type=argument|getNth:2 phase="argument" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}
	{{name}}(
		{% with arguments=symbols|fetch:name %}
			{% for argument in arguments %}
				{% if argument|is_argument %}
					{% with raw_type=argument|getNth:3 phase="argument" %}
						{% with type=raw_type|resolved:types %}
							{% with N=argument|getNth:2 %}
								{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}
									{% include "lib/builtin_type.tpl" %}
								{% endwith %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endif %}
				{% if not forloop.last and argument|is_argument %},{%endif%}
			{% endfor %}
		{% endwith %}
		);

	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_return %}
				{% with raw_type=argument|getNth:2 phase="to_erl" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with carg="c_retval" erlarg="retval" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}

	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_argument %}
				{% with raw_type=argument|getNth:3 phase="cleanup" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with carg="carg_"|add:N erlarg="argv["|add:N|add:"]" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}
	
	return retval;
error:
	return enif_make_badarg(env);
goto error;
	err++;
}

{% endfor %}{% endwith %}
