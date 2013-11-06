-module({{module}}).
-export([
{% for name, data in functions %}{% for rettype, args in data %}
	{{name}}/{{args|length}}{% if not forloop.parentloop.last %},{%endif%}
{% endfor %}{% endfor %}
]).
-on_load(init/0).

init() ->
	ok = erlang:load_nif("{{module}}_nif", 0).

{% for name, data in functions %}
{{name}}({% for rettype, args in data %}_{% endfor %}) ->
	exit(nif_library_not_loaded).
{% endfor %}
