-module(test_crc16).
-export([prop_comapre_reference/0]).

-include_lib("proper/include/proper.hrl").

prop_comapre_reference() -> 
	?FORALL(L, list(integer(0,255)), crc16:calc(L) =:= crc16c:crc16_data(L, length(L), 0)).
