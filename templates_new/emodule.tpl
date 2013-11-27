-module({{module}}).
-export([{% with fn=functions|fetch_keys %}{% for name in fn %}
	{{name}}/{{ functions|fetch:name|getNth:2|length }},{% endfor %}{% endwith %}
	free_ptr/1,
	list_to_cstr/1,
	cstr_to_list/1
	]).

-on_load(init/0).

init() ->
	ok = erlang:load_nif("{{module}}_nif", 0).

{% with fn=functions|fetch_keys %}{% for name in fn %}
{{name}}({% with arguments=symbols|fetch:name %}{% for argument in arguments %}{% if argument|is_argument %}_{% if not forloop.last %},{%endif%}{% endif %}{% endfor %}{% endwith %}) ->
	exit(nif_library_not_loaded).
{% endfor %}{% endwith %}

free_ptr(_) ->
	exit(nif_library_not_loaded).

list_to_cstr(_) ->
	exit(nif_library_not_loaded).

cstr_to_list(_) ->
	exit(nif_library_not_loaded).
