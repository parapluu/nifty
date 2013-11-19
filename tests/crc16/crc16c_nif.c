#include <erl_nif.h>

#include </home/thegeorge/sources/contiki/core/lib/crc16.h>

/*
 * conversion functions
 */







/*
 * Build Function Definitions
 */

static ERL_NIF_TERM
erl2c_crc16_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	/* make flag dependent */
	unsigned l;
	unsigned int conv_uint;
	int conv_int;
	ERL_NIF_TERM head, tail;
	
	unsigned short c_retval;
	ERL_NIF_TERM retval;
	
	
	unsigned char c_arg_0;
	
	unsigned short c_arg_1;
	

/*
 * build arguments
 */









	enif_get_uint(env, argv[0], &conv_uint);
	c_arg_0 = (unsigned char)conv_uint;










	enif_get_uint(env, argv[1], &conv_uint);
	c_arg_1 = (unsigned char)conv_uint;



/*
 * call the c function
 */
	c_retval = crc16_add(

	c_arg_0,

	c_arg_1

	);

/*
 * build return value
 */



	retval = enif_make_uint(env, c_retval);



/*
 * cleanup
 */

	

	

/*
 * return value
 */
	return retval;
/*
 * error handling
 */
}


static ERL_NIF_TERM
erl2c_crc16_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	/* make flag dependent */
	unsigned l;
	unsigned int conv_uint;
	int conv_int;
	ERL_NIF_TERM head, tail;
	
	unsigned short c_retval;
	ERL_NIF_TERM retval;
	
	
	const unsigned char * c_arg_0;
	
	int c_arg_1;
	
	unsigned short c_arg_2;
	

/*
 * build arguments
 */



	enif_get_list_length(env, argv[0], &l);
	c_arg_0 = enif_alloc(sizeof(const unsigned char *)*(l+1));
	enif_get_string(env, argv[0], c_arg_0, l+1, ERL_NIF_LATIN1);










	enif_get_int(env, argv[1], &c_arg_1);










	enif_get_uint(env, argv[2], &conv_uint);
	c_arg_2 = (unsigned char)conv_uint;



/*
 * call the c function
 */
	c_retval = crc16_data(

	c_arg_0,

	c_arg_1,

	c_arg_2

	);

/*
 * build return value
 */



	retval = enif_make_uint(env, c_retval);



/*
 * cleanup
 */

	enif_free(c_arg_0);

	

	

/*
 * return value
 */
	return retval;
/*
 * error handling
 */
}




/*
 * Function definitions for ErLang
 */
static ErlNifFunc nif_funcs[] = {


	{"crc16_add", 2, erl2c_crc16_add},


	{"crc16_data", 3, erl2c_crc16_data}

	};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

ERL_NIF_INIT(crc16c, nif_funcs, NULL, NULL, upgrade, NULL);
