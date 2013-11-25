{% with fn=functions|fetch_keys %}{% for name in fn %}

static ERL_NIF_TERM
erl2c_{{name}}(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err;
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
				{% with type=argument|getNth:3 phase="prepare" %}
					{% include "lib/builtin_type.tpl" %}
				{% endwith %}
			{% endif %}
			{% if argument|is_return %}
				{% with type=argument|getNth:2 phase="prepare" %}
					{% include "lib/builtin_type.tpl" %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}

	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_argument %}
				{% with type=argument|getNth:3 phase="to_c" %}
					{% include "lib/builtin_type.tpl" %}
	if (!err) {
		goto error;
	}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}

	c_retval =
	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_return %}
				{% with type=argument|getNth:2 phase="argument" %}
					{% include "lib/builtin_type.tpl" %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}
	{{name}}(
		{% with arguments=symbols|fetch:name %}
			{% for argument in arguments %}
				{% if argument|is_argument %}
					{% with type=argument|getNth:3 phase="argument" %}
						{% include "lib/builtin_type.tpl" %}
					{% endwith %}
				{% endif %}
				{% if not forloop.last and argument|is_argument %},{%endif%}
			{% endfor %}
		{% endwith %}
		);

	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_return %}
				{% with type=argument|getNth:2 phase="to_erl" %}
					{% include "lib/builtin_type.tpl" %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}

	{% with arguments=symbols|fetch:name %}
		{% for argument in arguments %}
			{% if argument|is_argument %}
				{% with type=argument|getNth:3 phase="cleanup" %}
					{% include "lib/builtin_type.tpl" %}
				{% endwith %}
			{% endif %}
		{% endfor %}
	{% endwith %}
error:
	return enif_make_badarg(env);
}

{% endfor %}{% endwith %}
