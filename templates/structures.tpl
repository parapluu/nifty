{% with type_keys=types|fetch_keys %}{% for type in type_keys %}{% with kind=types|fetch:type|getNth:1 %}{% if kind=="struct" %}
static ERL_NIF_TERM
ptr_to_record_{{type}}(ErlNifEnv* env, uint64_t ptr)
{
	return enif_make_atom(env, "ok");
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
	if (!(strcmp((const char*)cstr, "{{type}}"))) { return  record_to_erlptr_{{type}}(argv[0]); }
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
	if (!(strcmp((const char*)cstr, "{{type}}"))) { return  ptr_to_record_{{type}}(ptr); }
{% endif %}{% endwith%}{% endfor %}{% endwith %}

error:
	return enif_make_badarg(env);
}
