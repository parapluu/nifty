-module({{module}}).

-export([{% with fn=symbols|fetch_keys %}{% for name in fn %}
	'{{name}}'/{{ symbols|fetch:name|length|add:-1 }},{% endfor %}{% endwith %}
	erlptr_to_record/1,
	record_to_erlptr/1,
	erlptr_to_urecord/1,
	urecord_to_erlptr/1,
	'__nifty__get_types'/0,
	'__nifty__get_enum_aliases'/0,
	'__nifty__new'/1,
	'__nifty__size_of'/1]).

-define(TYPES, {{types}}).

-define(ENUM_ALIASES, {{constructors|enum_aliases}}).

-on_load(init/0).

-type addr()     :: integer().
-type typename() :: string().
-type ptr()      :: {addr(), typename()}.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "{{module}}_nif"), 0).

{% with fn=symbols|fetch_keys %}{% for name in fn %}'{{name}}'({% with arguments=symbols|fetch:name %}{% for argument in arguments %}{% if argument|is_argument %}_{% if not forloop.last %},{%endif%}{% endif %}{% endfor %}{% endwith %}) ->
	erlang:nif_error(nif_library_not_loaded).
{% endfor %}{% endwith %}

%%% static

-spec erlptr_to_record(ptr()) -> tuple().
erlptr_to_record(_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec record_to_erlptr(tuple()) -> ptr().
record_to_erlptr(_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec erlptr_to_urecord(ptr()) -> tuple().
erlptr_to_urecord(_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec urecord_to_erlptr(tuple()) -> ptr().
urecord_to_erlptr(_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec '__nifty__get_types'() -> nifty_clangparse:type_table().
'__nifty__get_types'() ->
    ?TYPES.

-spec '__nifty__get_enum_aliases'() -> proplist:proplist().
'__nifty__get_enum_aliases'() ->
    ?ENUM_ALIASES.

-spec '__nifty__new'(typename()) -> term().
'__nifty__new'(_) ->
    erlang:nif_error(nif_library_not_loaded).

-spec '__nifty__size_of'(typename()) -> integer() | undef.
'__nifty__size_of'(_) ->
    erlang:nif_error(nif_library_not_loaded).
