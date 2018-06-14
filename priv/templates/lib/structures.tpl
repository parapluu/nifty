{% if prototypes==1 %}
{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="struct"%}
static ERL_NIF_TERM ptr_to_record_{{name}}(ErlNifEnv* env, ptr_t ptr);
static ERL_NIF_TERM record_to_erlptr_{{name}}(ErlNifEnv* env, ERL_NIF_TERM record);
{% endif %}{% endwith%}{% endfor %}{% endwith %}

{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="union"%}
static ERL_NIF_TERM ptr_to_urecord_{{name}}(ErlNifEnv* env, ptr_t ptr);
static ERL_NIF_TERM urecord_to_erlptr_{{name}}(ErlNifEnv* env, ERL_NIF_TERM record);
{% endif %}{% endwith%}{% endfor %}{% endwith %}
{% else %}

{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="struct" %}
static ERL_NIF_TERM
ptr_to_record_{{name}}(ErlNifEnv* env, ptr_t ptr)
{
	{% if constructors|fetch:constr|length > 0 %}
	struct {{name}}* cstruct=(struct {{name}}*)ptr;
	{% endif %}
	ERL_NIF_TERM retval;
	{% with fields=constructors|fetch:constr %}
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



	{% with fields=constructors|fetch:constr %}
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

{% with fields=constructors|fetch:constr %}{% with tpl_length=fields|length|add:1 %}{% if tpl_length>9 %}
	retval = enif_make_tuple(env, {{tpl_length}}, enif_make_atom(
{% else %}
	retval = enif_make_tuple{{tpl_length}}(env, enif_make_atom(
{% endif %}{% endwith %}{% endwith %}
		env, "struct {{name}}")
	{% with fields=constructors|fetch:constr %}
		{% for argument in fields %}
						{% with N=argument|getNth:2 %}
							,{{"erlarg_"|add:N}}
						{% endwith %}
		{% endfor %}
	{% endwith %});

	return retval;
}

static ERL_NIF_TERM
record_to_erlptr_{{name}}(ErlNifEnv* env, ERL_NIF_TERM record)
{
	struct {{name}}* cstruct;
	int ar, err;
	ERL_NIF_TERM *tpl;

	{% with fields=constructors|fetch:constr %}
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

	cstruct = (struct {{name}}*)enif_alloc(sizeof(struct {{name}}));

	err = enif_get_tuple(env, record, &ar, (const ERL_NIF_TERM**)(&tpl));
	if (!err) {
		goto error;
	}

	{% with fields=constructors|fetch:constr %}
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
		nifty_make_ptr(env, (ptr_t)cstruct),
		enif_make_string(env, "{{module}}.struct {{name}} *", ERL_NIF_LATIN1));

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
{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="struct" %}
	if ((!(strcmp((const char*)cstr, "{{name}}")))
		|| (!(strcmp((const char*)cstr, "struct {{name}}")))) {
		return  record_to_erlptr_{{name}}(env, argv[0]);
	}
{% endif %}{% endwith%}{% endfor %}{% endwith %}



error:
	return enif_make_badarg(env);
}

static ERL_NIF_TERM
erlptr_to_record(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp;
	unsigned int l;
	ptr_t ptr;
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

	err = nifty_get_ptr(env, tpl[0], &ptr);
	if (!err) {
		goto error;
	}



{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="struct" %}
	if (!(strcmp((const char*)cstr, "{{name}}"))) { return  ptr_to_record_{{name}}(env, ptr); }
{% endif %}{% endwith%}{% endfor %}{% endwith %}

error:
	return enif_make_badarg(env);
}

{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="union" %}
static ERL_NIF_TERM
ptr_to_urecord_{{name}}(ErlNifEnv* env, ptr_t ptr)
{
	{% if constructors|fetch:constr|length > 0 %}
	union {{name}}* cunion=(union {{name}}*)ptr;
	{% endif %}
	ERL_NIF_TERM retval;
	{% with fields=constructors|fetch:constr %}
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



	{% with fields=constructors|fetch:constr %}
		{% for argument in fields %}
				{% with raw_type=argument|getNth:3 phase="to_erl" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 %}
							{% with carg="(cunion->"|add:N|add:")" erlarg="erlarg_"|add:N %}
								{% include "lib/builtin_type.tpl" %}
							{% endwith %}
						{% endwith %}
					{% endwith %}
				{% endwith %}
		{% endfor %}
	{% endwith %}

{% with fields=constructors|fetch:constr %}{% with tpl_length=fields|length|add:1 %}{% if tpl_length>9 %}
	retval = enif_make_tuple(env, {{tpl_length}}, enif_make_atom(
{% else %}
	retval = enif_make_tuple{{tpl_length}}(env, enif_make_atom(
{% endif %}{% endwith %}{% endwith %}
		env, "union {{name}}")
	{% with fields=constructors|fetch:constr %}
		{% for argument in fields %}
						{% with N=argument|getNth:2 %}
							,{{"erlarg_"|add:N}}
						{% endwith %}
		{% endfor %}
	{% endwith %});

	return retval;
}

static ERL_NIF_TERM
urecord_to_erlptr_{{name}}(ErlNifEnv* env, ERL_NIF_TERM record)
{
	union {{name}}* cunion;
	int ar, err;
	ERL_NIF_TERM *tpl;

	{% with fields=constructors|fetch:constr %}
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

	cunion = (union {{name}}*)enif_alloc(sizeof(union {{name}}));

	err = enif_get_tuple(env, record, &ar, (const ERL_NIF_TERM**)(&tpl));
	if (!err) {
		goto error;
	}

	{% with fields=constructors|fetch:constr %}
		{% for argument in fields %}
				{% with raw_type=argument|getNth:3 phase="to_c" %}
					{% with type=raw_type|resolved:types %}
						{% with N=argument|getNth:2 I=argument|getNth:4|add:1 %}
							{% with carg="(cunion->"|add:N|add:")" erlarg="(tpl["|add:I|add:"])" %}
	if (!enif_is_atom(env, {{erlarg}})) {
								{% include "lib/builtin_type.tpl" %}
	}
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
		nifty_make_ptr(env, (ptr_t)cunion),
		enif_make_string(env, "{{module}}.union {{name}} *", ERL_NIF_LATIN1));

error:
	return enif_make_badarg(env);
}
{% endif %}{% endwith%}{% endfor %}{% endwith %}


static ERL_NIF_TERM
urecord_to_erlptr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="union" %}
	if ((!(strcmp((const char*)cstr, "{{name}}")))
		|| (!(strcmp((const char*)cstr, "union {{name}}")))) {
		return  urecord_to_erlptr_{{name}}(env, argv[0]);
	}
{% endif %}{% endwith%}{% endfor %}{% endwith %}

error:
	return enif_make_badarg(env);
}

static ERL_NIF_TERM
erlptr_to_urecord(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp;
	unsigned int l;
	ptr_t ptr;
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

	err = nifty_get_ptr(env, tpl[0], &ptr);
	if (!err) {
		goto error;
	}



{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="union" %}
	if (!(strcmp((const char*)cstr, "{{name}}"))) { return  ptr_to_urecord_{{name}}(env, ptr); }
{% endif %}{% endwith%}{% endfor %}{% endwith %}

error:
	return enif_make_badarg(env);
}


static ERL_NIF_TERM
size_of(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp;
	unsigned int l;
	char* cstr;

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
/* structs and unions */
{% with type_keys=types|clear_function_pointers|fetch_keys %}
	{% for type in type_keys %}
		{% with kind=types|fetch:type|getNth:1 %}
			{% if kind=="base" or kind=="userdef" or kind=="typedef" %}
	if (!(strcmp((const char*)cstr, "{{type}}")))
	{
		return enif_make_ulong(env, sizeof({{type|discard_restrict}}));
	}
			{% endif %}
			{% if kind=="userdef" %}
			{% endif %}
		{% endwith%}
	{% endfor %}
{% endwith %}
	return enif_make_atom(env, "undef");
error:
	return enif_make_badarg(env);
}

static ERL_NIF_TERM
new_type_object(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int err, written, tmp;
	unsigned int l;
	char* cstr;
	ptr_t type_holder;
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
{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="struct" %}
	if ((!(strcmp((const char*)cstr, "{{name}}")))
		|| (!(strcmp((const char*)cstr, "struct {{name}}"))))
	{
		type_holder = (ptr_t)enif_alloc(sizeof(struct {{name}}));
		retval=ptr_to_record_{{name}}(env, type_holder);
		enif_free((void*)type_holder);
		return retval;
	}
{% endif %}{% endwith%}{% endfor %}{% endwith %}
/* unions */
{% with keys = constructors|fetch_keys %}{% for constr in keys %}{% with kind=constr|getNth:1 name=constr|getNth:2 %}{% if kind=="union" %}
	if ((!(strcmp((const char*)cstr, "{{name}}")))
		|| (!(strcmp((const char*)cstr, "union {{name}}"))))
	{
		type_holder = (ptr_t)enif_alloc(sizeof(union {{name}}));
		retval=ptr_to_urecord_{{name}}(env, type_holder);
		enif_free((void*)type_holder);
		return retval;
	}
{% endif %}{% endwith%}{% endfor %}{% endwith %}
	return enif_make_atom(env, "undef");
error:
	return enif_make_badarg(env);
/* supress warnings */
	type_holder++;
	retval++;
}
{% endif %}
