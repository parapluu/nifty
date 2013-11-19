-module(test_crc16).

-include_lib("proper/include/proper.hrl").

prop_compare_reference() -> 
	?FORALL({L,Start}, {list(integer(0,255)), integer(0,255)}, 
		crc16:calc(L, Start) =:= crc16c:crc16_data(L, length(L), Start)
	).
