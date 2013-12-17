-module(listlib).
-export([sumlist/1]).

sumlist (L) -> 
    sumlist(L, [1], 0).

sumlist ([], _, Sum) -> Sum;
sumlist ([H|T],  S, Sum) ->
    case S of
	[H|_] -> 
	    case length(S) of
		5 -> sumlist(T,[],Sum);
		_ -> sumlist(T, [H|S], Sum+H)
	    end;
	[] -> sumlist(T, [H], Sum+H);
	_ -> sumlist(T,S, Sum+H)
    end.
