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
							{% with carg="carg_"|add:N erlarg="erlarg_"|add:N %}
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
							{% with carg="carg_"|add:N erlarg="erlarg_"|add:N %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
		{% endfor %}
	{% endwith %}

	retval = enif_make_tuple(env, enif_make_string(env, "{{type}}", ERL_NIF_LATIN1),
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
	return enif_make_atom(env, "ok");
}
{% endif %}{% endwith%}{% endfor %}{% endwith %}


static ERL_NIF_TERM
record_to_erlptr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp;
	unsigned int l;
	char* cstr;
	
	err = enif_get_list_length(env, argv[0], &l);
	if (!err) {
		goto error;
	}
	cstr = enif_alloc(sizeof(char)*1);
	written = 0;
	while (written<(l)) {
		tmp = enif_get_string(env, argv[0], cstr+written, l-written, ERL_NIF_LATIN1);
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

	err = enif_get_list_length(env, argv[0], &l);
	if (!err) {
		goto error;
	}
	cstr = enif_alloc(sizeof(char)*1);
	written = 0;
	while (written<(l)) {
		tmp = enif_get_string(env, argv[0], cstr+written, l-written, ERL_NIF_LATIN1);
		if (tmp<=0) {
			enif_free(cstr);
			goto error;
		}
		written += tmp;
	}

	err = enif_get_uint64(env, argv[1], &ptr);
	if (!err) {
		goto error;
	}

{% with type_keys=types|fetch_keys %}{% for type in type_keys %}{% with kind=types|fetch:type|getNth:1 %}{% if kind=="struct" %}
	if (!(strcmp((const char*)cstr, "{{type}}"))) { return  ptr_to_record_{{type}}(env, ptr); }
{% endif %}{% endwith%}{% endfor %}{% endwith %}

error:
	return enif_make_badarg(env);
}
