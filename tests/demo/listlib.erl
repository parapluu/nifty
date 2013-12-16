-module(listlib).
-export([sumlist/1]).

sumlist (L) -> 
	sumlist(L, 0, 0).

sumlist ([], _, Sum) -> Sum;
sumlist ([H|T],  S, Sum) ->
	case H of
		1 ->
			case S of
				3 -> sumlist(T,0,Sum);
				_ -> sumlist(T,S+1, Sum+1)
			end;
		_ -> sumlist(T, 0, Sum+H)
	end.
