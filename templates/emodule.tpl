-module({{module}}).
-export([{% with fn=functions|fetch_keys %}{% for name in fn %}
	{{name}}/{{ functions|fetch:name|getNth:2|length }},{% endfor %}{% endwith %}
	get_types/0
	]).

-define(TYPES, {{types}}).

-on_load(init/0).

init() ->
	ok = erlang:load_nif("{{module}}_nif", 0).

{% with fn=functions|fetch_keys %}{% for name in fn %}
{{name}}({% with arguments=symbols|fetch:name %}{% for argument in arguments %}{% if argument|is_argument %}_{% if not forloop.last %},{%endif%}{% endif %}{% endfor %}{% endwith %}) ->
	exit(nif_library_not_loaded).
{% endfor %}{% endwith %}

get_types() -> ?TYPES.
