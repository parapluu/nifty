	enif_get_list_length(env, {{erlname}}, &l);
	{{cname}} = enif_alloc(sizeof(char)*(l+1));
	enif_get_string(env, {{erlname}}, (char*){{cname}}, l+1, ERL_NIF_LATIN1);
