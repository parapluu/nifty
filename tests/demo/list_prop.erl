-module(list_prop).
-include_lib("proper/include/proper.hrl").

prop_list() ->
	?FORALL(L,
		list(integer()),
		lists:sum(L)=:=listlib:sumlist(L)
		).
