-module(test).
-export([test/0, testa/0]).

compile() ->
    nifty_compiler:compile("/home/thegeorge/sources/contiki/apps/json/jsonparse.h", 
			   jsonparse,
			   [{port_specs,
			     [{".*",
			       "priv/jsonparse_nif.so",
			       ["/home/thegeorge/sources/contiki/apps/json/jsonparse.c"],
			       [{env, [{"CFLAGS",
					"$CFLAGS -I/home/thegeorge/projects/nifty/tests/contiki_json/"}]}]
			      }]
			    }]).

testa() ->
    compile(),
    test().

test() ->
    %%Json = "{\"employees\": [{ \"firstName\":\"John\" , \"lastName\":\"Doe\" }, { \"firstName\":\"Anna\" , \"lastName\":\"Smith\" }, { \"firstName\":\"Peter\" , \"lastName\":\"Jones\" }]}",
    Json = "{\"a\": ",
    JsonPtr = nifty:list_to_cstr(Json),
    State = jsonparse:new("jsonparse_state"),
    StatePtr = jsonparse:record_to_erlptr(State),
    jsonparse:jsonparse_setup(StatePtr, JsonPtr, length(Json)),
    E = next_till_error(StatePtr, 0, []),
    {E, StatePtr}.

next_till_error(S, N, Ret) ->
    case jsonparse:jsonparse_has_next(S) of
	0 -> {0, N, lists:reverse(Ret)};
	1 -> R = case jsonparse:jsonparse_next(S) of
		     0 -> Ret;
		     C -> [C|Ret]
		 end,
	     case nifty:dereference(S) of
		 {_,_,_,_,_,_,_,_,{0,_},_} -> next_till_error(S, N+1, R);
		 {_,_,_,_,_,_,_,_,{E,_},_} -> {E, N, lists:reverse(R)}
	     end
    end.


%% jsonparse_next
%%   return value:
%%     type
%%       JSON_TYPE_ARRAY '['
%%       JSON_TYPE_OBJECT '{'
%%       JSON_TYPE_PAIR ':'
%%       JSON_TYPE_PAIR_NAME 'N' for N:V pair 
%%       JSON_TYPE_STRING '"'
%%       JSON_TYPE_INT 'I'
%%       JSON_TYPE_NUMBER '0'
%%     0 if not finished yet

%% enum {
%%   JSON_ERROR_OK,
%%   JSON_ERROR_SYNTAX,
%%   JSON_ERROR_UNEXPECTED_ARRAY,
%%   JSON_ERROR_UNEXPECTED_END_OF_ARRAY,
%%   JSON_ERROR_UNEXPECTED_OBJECT,
%%   JSON_ERROR_UNEXPECTED_STRING
%% };
