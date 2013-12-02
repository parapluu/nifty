#include <erl_nif.h>
#include <stdint.h>

#include <stdio.h>

static ERL_NIF_TERM
raw_free(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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


static ERL_NIF_TERM
raw_deref(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err;
	uint64_t ptr;
	err = enif_get_uint64(env, argv[0], &ptr);
	if (!err) {
			goto error;
	}
	return enif_make_int64(env, (*(uint64_t*)ptr));
error:
	return enif_make_badarg(env);
}

static ERL_NIF_TERM
cstr_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, ar;
	uint64_t ptr;
	ERL_NIF_TERM *tpl;

	err = enif_get_tuple(env, argv[0], &ar, (const ERL_NIF_TERM**)(&tpl));
	if (err) {
		err = enif_get_uint64(env, tpl[0], &ptr);
	}
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

	return enif_make_tuple3(
		env,
		enif_make_int64(env, (uint64_t)cstr),
		enif_make_atom(env, "nifty"),
		enif_make_string(env, "char *", ERL_NIF_LATIN1));

error:
	return enif_make_badarg(env);
}


static ErlNifFunc nif_funcs[] = {
  {"raw_deref", 1, raw_deref},
  {"raw_free", 1, raw_free},
  {"list_to_cstr", 1, list_to_cstr},
  {"cstr_to_list", 1, cstr_to_list}
};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  return 0;
}

ERL_NIF_INIT(nifty, nif_funcs, NULL, NULL, upgrade, NULL);
