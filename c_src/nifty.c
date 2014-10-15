/*
 * Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
 *                     Konstantinos Sagonas <kostis@it.uu.se>
 * All rights reserved.
 * 
 * This file is distributed under the Simplified BSD License.
 * Details can be found in the LICENSE file.
 */

#include <erl_nif.h>
#include <stdio.h>
#include <string.h>

#if _WIN32 || _WIN64
	#if _WIN64
typedef unsigned __int64 uint64_t;
#define ENV64BIT
	#else
#define ENV32BIT
	#endif
#else // clang gcc
#include <stdint.h>
	#if __x86_64__
		#define ENV64BIT
	#else
		#define ENV32BIT
	#endif
#endif 

#ifdef ENV32BIT
typedef unsigned long ptr_t;
#define nifty_get_ptr(env, term, ip) enif_get_ulong((env), (term), (ip))
#define nifty_make_ptr(env, i) enif_make_ulong((env), (i))
#else /* ENV64BIT */
typedef uint64_t ptr_t;
#define nifty_get_ptr(env, term, ip) enif_get_uint64((env), (term), (ip))
#define nifty_make_ptr(env, i) enif_make_uint64((env), (i))
#endif


static ERL_NIF_TERM
raw_free(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err;
  ptr_t ptr;
  err = nifty_get_ptr(env, argv[0], &ptr);
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
  ptr_t ptr;
  err = nifty_get_ptr(env, argv[0], &ptr);
  if (!err) {
    goto error;
  }
  return enif_make_int64(env, (*(ptr_t*)ptr));
 error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
float_deref(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err;
  ptr_t ptr;
  err = nifty_get_ptr(env, argv[0], &ptr);
  if (!err) {
    goto error;
  }
  return enif_make_double(env, ((double)(*((float*)ptr))));
error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
float_ref(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err;
  double helper;
  float *value;
  
  value = enif_alloc(sizeof(float));
  if (!value) {
    goto error;
  }

  err = enif_get_double(env, argv[0], &helper);
  if (!err) {
    enif_free(value);
    goto error;
  }

  *value = (float)helper;
  
  return enif_make_tuple2(env, 
			  nifty_make_ptr(env, (ptr_t)value),
			  enif_make_string(env, "nifty.float *", ERL_NIF_LATIN1));

 error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
double_deref(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err;
  ptr_t ptr;
  err = nifty_get_ptr(env, argv[0], &ptr);
  if (!err) {
    goto error;
  }
  return enif_make_double(env, *((double*)ptr));
error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
double_ref(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err;
  double *value;
  
  value = enif_alloc(sizeof(double));
  if (!value) {
    goto error;
  }

  err = enif_get_double(env, argv[0], value);
  if (!err) {
    enif_free(value);
    goto error;
  }

  return enif_make_tuple2(env, 
			  nifty_make_ptr(env, (ptr_t)value),
			  enif_make_string(env, "nifty.double *", ERL_NIF_LATIN1));

 error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
cstr_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err, ar;
  ptr_t ptr;
  ERL_NIF_TERM *tpl;

  err = enif_get_tuple(env, argv[0], &ar, (const ERL_NIF_TERM**)(&tpl));
  if (err) {
    err = nifty_get_ptr(env, tpl[0], &ptr);
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
  cstr = enif_alloc(sizeof(char)*l);
  if (!cstr) {
    goto error;
  }
  written = 0;
  while (written<(l)) {
    tmp = enif_get_string(env, argv[0], cstr+written, l-written, ERL_NIF_LATIN1);
    if (tmp<=0) {
      enif_free(cstr);
      goto error;
    }
    written += tmp;
  }

  return enif_make_tuple2(env,
			  enif_make_int64(env, (ptr_t)cstr),
			  enif_make_string(env, "nifty.char *", ERL_NIF_LATIN1));

 error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
mem_write_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int i, l, data, err;
  int ar;
  char* ptr;
  ERL_NIF_TERM head, tail, list, *tpl;

  err = enif_get_list_length(env, argv[0], &l);
  if (!err) {
    goto error;
  }

	
  err = enif_get_tuple(env, argv[1], &ar, (const ERL_NIF_TERM**)(&tpl));
  if (err) {
    err = nifty_get_ptr(env, tpl[0], (ptr_t*)&ptr);
  }
  if (!err) {
    goto error;
  }

  list = argv[0];
  i = 0;

  while (!enif_is_empty_list(env, list)) {
    err = enif_get_list_cell(env, list, &head, &tail);
    list = tail;
    if (!err) {
      goto error;
    }
    err = enif_get_uint(env, head, &data);
    if (!err) {
      goto error;
    }
    *(ptr+i++) = (char)data;
  }

  return argv[1];

 error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
mem_write_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int err;
  int ar;
  unsigned char* ptr;
  ERL_NIF_TERM *tpl;
  ErlNifBinary bin;

  if (!enif_inspect_binary(env, argv[0], &bin)) {
    goto error;
  }

  err = enif_get_tuple(env, argv[1], &ar, (const ERL_NIF_TERM**)(&tpl));
  if (err) {
    err = nifty_get_ptr(env, tpl[0], (ptr_t*)&ptr);
  }
  if (!err) {
    goto error;
  }

  memcpy(ptr, bin.data, bin.size);

  return argv[1];

 error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
mem_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int err, l, i, tmp;
  char element;
  int ar;
  char* ptr;
  ERL_NIF_TERM list, head, *tpl;


  err = enif_get_tuple(env, argv[0], &ar, (const ERL_NIF_TERM**)(&tpl));
  if (err) {
    err = nifty_get_ptr(env, tpl[0], (ptr_t*)&ptr);
  }
  if (!err) {
    goto error;
  }

  err = enif_get_uint(env, argv[1], &l);
  if (!err) {
    goto error;
  }

  list = enif_make_list(env, 0);

  for (i=0;i<l;i++) {
    element = (char)*(ptr+(l-1)-i);
    tmp = element & 0xff;
    head = enif_make_uint(env, tmp);
    list = enif_make_list_cell(env, head, list);
  }
  return list;
 error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
mem_alloc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ptr_t ptr, size, err;

  err = nifty_get_ptr(env, argv[0], &size);
  if (!err) {
    goto error;
  }

  ptr = (ptr_t)enif_alloc(size);
  if (!ptr) {
    goto error;
  }

  return enif_make_tuple2(env,
			  nifty_make_ptr(env, (ptr_t)ptr),
			  enif_make_string(env, "nifty.void *", ERL_NIF_LATIN1));

 error:
  return enif_make_badarg(env);
}

static ERL_NIF_TERM
get_config(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  return enif_make_list2(env,
			 enif_make_tuple2(env,
					  enif_make_string(env,"sizes", ERL_NIF_LATIN1),
					  enif_make_tuple6(env,
							   enif_make_uint(env, sizeof(short int)),
							   enif_make_uint(env, sizeof(int)),
							   enif_make_uint(env, sizeof(long int)),
							   enif_make_uint(env, sizeof(long long int)),
							   enif_make_uint(env, sizeof(float)),
							   enif_make_uint(env, sizeof(double)))),
			 enif_make_tuple2(env,
					  enif_make_string(env, "arch", ERL_NIF_LATIN1),
					  enif_make_tuple2(env,
#ifdef ENV64BIT
							   enif_make_string(env, "64bit", ERL_NIF_LATIN1),
#endif
#ifdef ENV32BIT
							   enif_make_string(env, "32bit", ERL_NIF_LATIN1),
#endif
							   enif_make_uint(env, sizeof(char*)))));
}

static ERL_NIF_TERM
get_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  return enif_make_tuple2(env, 
			  nifty_make_ptr(env, (ptr_t)env),
			  enif_make_string(env, "nifty.void *", ERL_NIF_LATIN1));
}


static ErlNifFunc nif_funcs[] = {
  {"raw_deref", 1, raw_deref},
  {"float_deref", 1, float_deref},
  {"float_ref", 1, float_ref},
  {"double_deref", 1, double_deref},
  {"double_ref", 1, double_ref},
  {"raw_free", 1, raw_free},
  {"list_to_cstr", 1, list_to_cstr},
  {"cstr_to_list", 1, cstr_to_list},
  {"mem_write_list", 2, mem_write_list},
  {"mem_write_binary", 2, mem_write_binary},
  {"mem_read", 2, mem_read},
  {"mem_alloc", 1, mem_alloc},
  {"get_config", 0, get_config},
  {"get_env", 0, get_env}
};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  return 0;
}

ERL_NIF_INIT(nifty, nif_funcs, NULL, NULL, upgrade, NULL);
