static ERL_NIF_TERM
free_pointer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err;
	uint64_t ptr;
		err = enif_get_uint64(env, argv[0], &ptr);
	if (!err) {
			goto error;
	}
	enif_free((void*)ptr);
	return enif_make_atom(env, "ok");
error:
	return enif_make_badarg(env);
}


