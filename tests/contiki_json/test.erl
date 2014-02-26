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
    Json = "{\"employees\": [{ \"firstName\":\"John\" , \"lastName\":\"Doe\" }, { \"firstName\":\"Anna\" , \"lastName\":\"Smith\" }, { \"firstName\":\"Peter\" , \"lastName\":\"Jones\" }]}",
    JsonPtr = nifty:list_to_cstr(Json),
    State = jsonparse:new("jsonparse_state"),
    StatePtr = jsonparse:record_to_erlptr(State),
    jsonparse:jsonparse_setup(StatePtr, JsonPtr, length(Json)),
    StatePtr.
