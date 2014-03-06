-module(test_crc16).

-include_lib("proper/include/proper.hrl").

setup() ->
	nifty_compiler:compile("./crc16.h", crc16c, 
		[{port_specs, 
				[{".*", "$NIF", ["./crc16_fixed.c"]}]}]).


prop_compare_reference() -> 
	begin
		setup(),
		?FORALL({L,Start}, {list(integer(44,45)), integer(0,255)}, 
			crc16:calc(L, Start) =:= crc16c:crc16_data(nifty:list_to_cstr(L), length(L), Start)
		)
	end.
