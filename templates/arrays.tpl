{% if prototypes==1 %}
{% with type_keys=types|fetch_keys %}{% for type in type_keys %}{% with typedef=types|fetch:type|getNth:2 %}{% if typedef|getNth:1|is_array %}
static int lst_to_array_{{typedef|array_name}}(ErlNifEnv* env, ERL_NIF_TERM erlarg, {{type|array_get_base_type}}* ptr, unsigned int length);
static ERL_NIF_TERM array_to_lst_{{typedef|array_name}}(ErlNifEnv* env, {{type|array_get_base_type}} ptr, unsigned int length);
{% endif %}{% endwith %}{% endfor %}{% endwith %}

{% else %}

{% with type_keys=types|fetch_keys %}{% for type in type_keys %}{% with typedef=types|fetch:type|getNth:2 %}{% if typedef|getNth:1|is_array %}
static int
lst_to_array_{{typedef|array_name}}(ErlNifEnv* env, ERL_NIF_TERM erlarg, {{type|array_get_base_type}}* ptr, unsigned int length)
{
    /*
     * from erl to c
     */
     return 0;
}

static ERL_NIF_TERM
array_to_lst_{{typedef|array_name}}(ErlNifEnv* env, {{type|array_get_base_type}} ptr, unsigned int length)
{
    /*
     * from c to erl
     */
     return enif_make_atom(env, "ok");
}
{% endif %}{% endwith %}{% endfor %}{% endwith %}
{% endif %}
