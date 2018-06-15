-module({{module}}_remote).

-export([{% with fn=symbols|fetch_keys %}{% for name in fn %}
	'{{name}}'/{{ symbols|fetch:name|length|add:-1 }},{% endfor %}{% endwith %}
	start/0,
	stop/0,
	restart/0,
	erlptr_to_record/1,
	record_to_erlptr/1,
	'__nifty__get_types'/0,
	'__nifty__new'/1]).

-type addr() :: integer().
-type typename() :: string().
-type ptr() :: {addr(), typename()}.
-type reason() :: atom().
-type error() :: {error, reason()}.

%%% Generated

{% with fn=symbols|fetch_keys %}{% for name in fn %}'{{name}}'({% with arguments=symbols|fetch:name %}{% for argument in arguments %}{% if argument|is_argument %}X{{forloop.counter0}}{% if not forloop.last %},{%endif%}{% endif %}{% endfor %}) ->
	nifty_remotecall:call_remote({{module}}, '{{name}}', [{% for argument in arguments %}{% if argument|is_argument %}X{{forloop.counter0}}{% if not forloop.last %},{%endif%}{% endif %}{% endfor %}]).
{% endwith %}{% endfor %}{% endwith %}

%%% static

start() ->
    nifty_remotecall:start().

stop() ->
    nifty_remotecall:stop().

restart() ->
    nifty_remotecall:restart().

-spec erlptr_to_record(ptr()) -> tuple() | error().
erlptr_to_record(Ptr) ->
    nifty_remotecall:call_remote({{module}}, erlptr_to_record, [Ptr]).

-spec record_to_erlptr(tuple()) -> ptr() | error().
record_to_erlptr(Rec) ->
    nifty_remotecall:call_remote({{module}}, record_to_erlptr, [Rec]).

-spec '__nifty__get_types'() -> nifty_clangparse:type_table() | error().
'__nifty__get_types'() ->
    nifty_remotecall:call_remote({{module}}, '__nifty__get_types', []).

-spec '__nifty__new'(typename()) -> term().
'__nifty__new'(Type) ->
    nifty_remotecall:call_remote({{module}}, '__nifty__new', [Type]).
