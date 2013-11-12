{% for sname, members in structs %}
ERL_NIF_TERM nifty_struct2tuple_{{sname}}(ErlNifEnv* env, struct {{sname}} * value);
struct {{sname}} * nifty_tuple2struct_{{sname}}(ErlNifEnv* env, ERL_NIF_TERM value);{% endfor %}

{% for sname, members in structs %}
ERL_NIF_TERM nifty_struct2tuple_{{sname}}(ErlNifEnv* env, struct {{sname}} * value)
{
	ERL_NIF_TERM retval;
	{% for packed in members %}{% for name, type in packed %}
	ERL_NIF_TERM erl_{{name}};
	{% endfor %}{% endfor %}
	if (!value) {
		return enif_make_atom(env, "null");
	}
	
	{% for packed in members %}{% for mname, type in packed %}
{% include "to_erl/build.tpl" with erlname="erl_"|add:mname cname="(value->"|add:mname|add:")" %}{% endfor %}{% endfor %}

	retval = enif_make_tuple(
		env,
		{% for packed in members %}{{packed|length}},
		{% for mname, type in packed %}erl_{{mname}}{% if not forloop.last %},{%endif%}
		{% endfor %}{% endfor %}
	);

	return retval;
}
{% endfor %}

{% for sname, members in structs %}
struct {{sname}} * nifty_tuple2struct_{{sname}}(ErlNifEnv* env, ERL_NIF_TERM value)
{
	struct {{sname}} *retval;
	int tuple_length;
	{% for packed in members %}{% for name, type in packed %}
	{{type}} c_{{name}};
	{% endfor %}
	ERL_NIF_TERM *tuple_access;{% endfor %}

	if (enif_is_identical(value, enif_make_atom(env, "null"))) {
		return NULL;
	}

	enif_get_tuple(env, value, &tuple_length, (ERL_NIF_TERM const **)(&tuple_access));

	retval = enif_alloc(sizeof(struct {{sname}}));
	{% for packed in members %}{% for mname, type in packed %}
{% include "from_erl/build.tpl" with erlname="(tuple_access["|add:forloop.counter|add:"])" cname="(retval->"|add:mname|add:")" %}{% endfor %}{% endfor %}

	return retval;
}
{% endfor %}