-module(crc16c).
-export([

	crc16_add/2,

	crc16_data/3

]).
-on_load(init/0).

init() ->
	ok = erlang:load_nif("crc16c_nif", 0).


crc16_add(_,_) ->
	exit(nif_library_not_loaded).

crc16_data(_,_,_) ->
	exit(nif_library_not_loaded).

