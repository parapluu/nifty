{% if prototypes==1 %}
{% with type_keys=types|fetch_keys %}{% for type in type_keys %}{% with kind=types|fetch:type|getNth:1 %}{% if kind=="struct" %}
static ERL_NIF_TERM ptr_to_record_{{type}}(ErlNifEnv* env, uint64_t ptr);
static ERL_NIF_TERM record_to_erlptr_{{type}}(ErlNifEnv* env, ERL_NIF_TERM record);
{% endif %}{% endwith%}{% endfor %}{% endwith %}

{% else %}


{% with type_keys=types|fetch_keys %}{% for type in type_keys %}{% with kind=types|fetch:type|getNth:1 %}{% if kind=="struct" %}
static ERL_NIF_TERM
ptr_to_record_{{type}}(ErlNifEnv* env, uint64_t ptr)
{
	struct {{type}}* cstruct=(struct {{type}}*)ptr;
	ERL_NIF_TERM retval;
	{% with fields=types|fetch:type|getNth:2 %}
		{% for argument in fields %}
				{% with raw_type=argument|getNth:3 phase="prepare" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with erlarg="erlarg_"|add:N carg=N record="to_record" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
		{% endfor %}
	{% endwith %}



	{% with fields=types|fetch:type|getNth:2 %}
		{% for argument in fields %}
				{% with raw_type=argument|getNth:3 phase="to_erl" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with carg="(cstruct->"|add:N|add:")" erlarg="erlarg_"|add:N %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
		{% endfor %}
	{% endwith %}

{% with fields=types|fetch:type|getNth:2 %}{% with tpl_length=fields|length|add:1 %}{% if tpl_length>9 %}
	retval = enif_make_tuple(env, {{tpl_length}}, enif_make_atom(
{% else %}
	retval = enif_make_tuple{{tpl_length}}(env, enif_make_atom(
{% endif %}{% endwith %}{% endwith %}
		env, "{{type|resolved:types}}"),
	{% with fields=types|fetch:type|getNth:2 %}
		{% for argument in fields %}
						{% with N=argument|getNth:2 %}
							{{"erlarg_"|add:N}}{% if not forloop.last %},{% endif %}
						{% endwith %}
		{% endfor %}
	{% endwith %});

	return retval;
}

static ERL_NIF_TERM
record_to_erlptr_{{type}}(ErlNifEnv* env, ERL_NIF_TERM record)
{
	struct {{type}}* cstruct;
	int ar, err;
	ERL_NIF_TERM *tpl;

	{% with fields=types|fetch:type|getNth:2 %}
		{% for argument in fields %}
				{% with raw_type=argument|getNth:3 phase="prepare" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with record="to_ptr" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
		{% endfor %}
	{% endwith %}

	cstruct = (struct {{type}}*)enif_alloc(sizeof(struct {{type}}*));

	err = enif_get_tuple(env, record, &ar, (const ERL_NIF_TERM**)(&tpl));
	if (!err) {
		goto error;
	}

	{% with fields=types|fetch:type|getNth:2 %}
		{% for argument in fields %}
				{% with raw_type=argument|getNth:3 phase="to_c" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 I=argument|getNth:4|add:1 %}
							{% with carg="(cstruct->"|add:N|add:")" erlarg="(tpl["|add:I|add:"])" %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
	if (!err) {
		goto error;
	}
				{% endwith %}
		{% endfor %}
	{% endwith %}

	return enif_make_tuple2(
		env,
		enif_make_uint64(env, (uint64_t)cstruct),
		enif_make_string(env, "{{module}}.struct {{type}} *", ERL_NIF_LATIN1));

error:
	return enif_make_badarg(env);
}
{% endif %}{% endwith%}{% endfor %}{% endwith %}


static ERL_NIF_TERM
record_to_erlptr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp, ar;
	unsigned int l;
	char* cstr;
	ERL_NIF_TERM *tpl;
	
	err = enif_get_tuple(env, argv[0], &ar, (const ERL_NIF_TERM**)(&tpl));
	if (!err) {
		goto error;
	}

	err = enif_get_atom_length(env, tpl[0], &l, ERL_NIF_LATIN1);
	if (!err) {
		goto error;
	}

	l+=1;
	cstr = enif_alloc(sizeof(char)*l);
	written = 0;
	while (written<(l)) {
		tmp = enif_get_atom(env, tpl[0], cstr+written, l-written, ERL_NIF_LATIN1);
		if (tmp==-(l-written)) {
				tmp=-tmp;
		}
		if (tmp<=0) {
			enif_free(cstr);
			goto error;
		}
		written += tmp;
	}
{% with type_keys=types|fetch_keys %}{% for type in type_keys %}{% with kind=types|fetch:type|getNth:1 %}{% if kind=="struct" %}
	if (!(strcmp((const char*)cstr, "{{type}}"))) { return  record_to_erlptr_{{type}}(env, argv[0]); }
{% endif %}{% endwith%}{% endfor %}{% endwith %}

error:
	return enif_make_badarg(env);
}

static ERL_NIF_TERM
erlptr_to_record(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp;
	unsigned int l;
	uint64_t ptr;
	char* cstr;

	ERL_NIF_TERM *tpl;

	err = enif_get_tuple(env, argv[0], &tmp, (const ERL_NIF_TERM**)(&tpl));
	if (!err) {
		goto error;
	}

	err = enif_get_list_length(env, tpl[1], &l);
	if (!err) {
		goto error;
	}

	l+=1;
	cstr = enif_alloc(sizeof(char)*l);
	written = 0;
	while (written<(l)) {
		tmp = enif_get_string(env, tpl[1], cstr+written, l-written, ERL_NIF_LATIN1);
		if (tmp==-(l-written)) {
			tmp=-tmp;
		}
		if (tmp<=0) {
			enif_free(cstr);
			goto error;
		}
		written += tmp;
	}

	err = enif_get_uint64(env, tpl[0], &ptr);
	if (!err) {
		goto error;
	}



{% with type_keys=types|fetch_keys %}{% for type in type_keys %}{% with kind=types|fetch:type|getNth:1 %}{% if kind=="struct" %}
	if (!(strcmp((const char*)cstr, "{{type}}"))) { return  ptr_to_record_{{type}}(env, ptr); }
{% endif %}{% endwith%}{% endfor %}{% endwith %}

error:
	return enif_make_badarg(env);
}

static ERL_NIF_TERM
new_type_object(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp;
	unsigned int l;
	char* cstr;
	uint64_t type_holder;
	ERL_NIF_TERM retval;

	err = enif_get_list_length(env, argv[0], &l);
	if (!err) {
		goto error;
	}

	l+=1;
	cstr = enif_alloc(sizeof(char)*l);
	written = 0;
	while (written<(l)) {
		tmp = enif_get_string(env, argv[0], cstr+written, l-written, ERL_NIF_LATIN1);
		if (tmp==-(l-written)) {
			tmp=-tmp;
		}
		if (tmp<=0) {
			enif_free(cstr);
			goto error;
		}
		written += tmp;
	}
/* structs */
{% with type_keys=types|fetch_keys %}
	{% for type in type_keys %}
		{% with kind=types|fetch:type|getNth:1 %}
			{% if kind=="struct" %}
	if ((!(strcmp((const char*)cstr, "{{type}}")))
		|| (!(strcmp((const char*)cstr, "struct {{type}}"))))
	{
		type_holder = (uint64_t)enif_alloc(sizeof(struct {{type}}));
		retval=ptr_to_record_{{type}}(env, type_holder);
		enif_free((void*)type_holder);
		return retval;
	}
			{% endif %}
			{% if kind=="userdef" %}
			{% endif %}
		{% endwith%}
	{% endfor %}
{% endwith %}
error:
	return enif_make_badarg(env);
/* supress warnings */
	type_holder++;
	retval++;
}
{% endif %}