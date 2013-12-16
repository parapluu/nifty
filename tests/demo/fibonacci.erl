-module(fibonacci).
-export([
	fib/1,
	fast_fib/1
	]).

fib (0) -> 0;
fib (1) -> 1;
fib (N) -> fib(N-1) + fib(N-2).

fast_fib (N) -> fast_fib(0,1,N).

fast_fib (Result, _, 0) -> Result;
fast_fib (N2, N1, ToGo) ->
	fast_fib(N1, N1+N2, ToGo-1).

