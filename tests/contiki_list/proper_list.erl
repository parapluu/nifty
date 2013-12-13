-module(proper_list).
-export([
	test/0,
	llist/1,
	item/0,
	create_list/0,
	next_state/3,
	initial_state/0,
	command/1,
	precondition/2,
	postcondition/3
	]).

-include_lib("proper/include/proper.hrl").

-type llist() :: term().
-record(state, {ls :: [llist()]}).

% generator
item() ->
	?LET(D, integer(), c_list:record_to_erlptr({list_item, null, D})).

% helper function
create_list() ->
	LIST = nifty:as_type(nifty:pointer(), c_list, "list_t"),
	c_list:list_init(LIST),
	LIST.


llist(#state{ls = Lists}) ->
	elements(Lists).

initial_state() ->
	#state{ls=[]}.

command(S) ->
	Lists = (S#state.ls =/= []),
	oneof(
		[{call, proper_list, create_list, []}] ++
		[{call, c_list, list_add, [llist(S), item()]} || Lists] ++
		[{call, c_list, list_pop, [llist(S)]} || Lists]
		).

next_state(S, V, {call, _, create_list, _}) ->
	S#state{ls=[V|S#state.ls]};
next_state(S,_,_) ->
	S.


precondition(_,_) -> true.

postcondition(_, {call, _, list_pop, _}, Result) ->
	case Result of	
		{0, _} -> true;
		Pointer ->
 			{list_item, _ , _ } = nifty:dereference(nifty:as_type(Pointer, c_list, "struct list_item *")),
			nifty:free(Pointer),
			true
	end;
postcondition(_,_,Result) ->
	case Result of	
		ok -> true;
		{0, _} -> true;
		Pointer ->
			{list_item, _ , _ } = nifty:dereference(nifty:as_type(Pointer, c_list, "struct list_item *")),
			true
	end.

prop_list_works_fine() ->
	?FORALL(Cmds, commands(?MODULE),
		?TRAPEXIT(
			begin
				{History,State,Result} = run_commands(?MODULE, Cmds),
				true,
				?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                        [History,State,Result]),
                              Result =:= ok)
			end)).

test() ->
	proper:quickcheck(?MODULE:prop_list_works_fine(), 1000).
