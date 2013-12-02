static ERL_NIF_TERM
cstr_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err;
	uint64_t ptr;

	err = enif_get_uint64(env, argv[0], &ptr);
	if (!err) {
		goto error;
	}
	return enif_make_string(env, (char*)ptr, ERL_NIF_LATIN1);

error:
	return enif_make_badarg(env);
}

static ERL_NIF_TERM
list_to_cstr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp;
	unsigned int l;
	char* cstr;

	err = enif_get_list_length(env, argv[0], &l);
	if (!err) {
		goto error;
	}
	l+=1; // Null Termination
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

	return enif_make_int64(env, (uint64_t)cstr);

error:
	return enif_make_badarg(env);
}
