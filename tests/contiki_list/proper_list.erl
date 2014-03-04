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

-type llist() :: {integer(), string()}.
-record(state, {ls :: [llist()], ref::list()}).

%% compiler
setup() ->
    nifty_compiler:compile("./list.h", c_list, 
    			   [{port_specs,
    			     [{
    						".*",
    						"priv/c_list_nif.so",	
    						["$CONTIKI/core/lib/list.c"],
    						[{env, [{"CFLAGS",
    							 "$CFLAGS -I$CONTIKI/core"}]}]
    				}]
    		}]).

%% generator
item() ->
	?LET(D, integer(), c_list:record_to_erlptr({list_item, null, D})).

%% helper function
create_list() ->
    LIST = nifty:as_type(nifty:pointer(), c_list, "list_t"),
    c_list:list_init(LIST),
    LIST.

llist(#state{ls = Lists}) ->
    elements(Lists).


initial_state() ->
    #state{ls=[], ref=dict:new()}.

command(S) ->
    NonEmpty = (S#state.ls =/= []),
    oneof([{call, ?MODULE, create_list, []}] ++
	      [{call, c_list, list_add, [llist(S), item()]} || NonEmpty] ++
	      [{call, c_list, list_push, [llist(S), item()]} || NonEmpty] ++
	      %% [{call, c_list, list_length, [llist(S)]} || NonEmpty] ++ %% can't test because there is an otp function with this name	    
	      [{call, c_list, list_pop, [llist(S)]} || NonEmpty] ++
	      [{call, c_list, list_chop, [llist(S)]} || NonEmpty]
	 ).

next_state(S, V, {call, _, create_list, _}) ->
    S#state{ls  = [V|S#state.ls], 
	    ref = dict:store(V, 
			   [], 
			   S#state.ref
			  )};
next_state(S, _, {call, _, list_add, [L, I]}) ->
    EL = dict:fetch(L, S#state.ref),
    {list_item, _, D} = nifty:dereference(I),
    S#state{ref = dict:store(L, EL ++ [D], S#state.ref)};
next_state(S, _, {call, _, list_push, [L, I]}) ->
    EL = dict:fetch(L, S#state.ref),
    {list_item, _, D} = nifty:dereference(I),
    S#state{ref = dict:store(L, [D|EL], S#state.ref)};
next_state(S, R, {call, _, list_pop, [L]}) ->
    case R of
	{var, _} -> %% don't care about the symbolic result
	    S;
	{0, _} ->   %% null pointer means list was already empty so ignore
	    S;
	_ ->    %% remove head element
	    [_|T] = dict:fetch(L, S#state.ref),
	    S#state{ref = dict:store(L, T, S#state.ref)}
    end;
next_state(S, R, {call, _, list_chop, [L]}) ->
    case R of
	{var, _} -> %% don't care about the symbolic result
	    S;
	{0, _} ->   %% null pointer means list was already empty so ignore
	    S;
	_ ->    %% remove head element
	    EL = dict:fetch(L, S#state.ref),
	    [_| TEL] = lists:reverse(EL),
	    S#state{ref = dict:store(L, lists:reverse(TEL), S#state.ref)}
    end;
next_state(S, _, _) ->
    S.

precondition(_, _) -> 
    true.

postcondition(_, {call, _, list_add, _}, _) ->
    true;
postcondition(_, {call, _, list_push, _}, _) ->
    true;
postcondition(S, {call, _, list_pop, [L]}, Result) ->
    EL = dict:fetch(L, S#state.ref),
    case Result of	
	{0, _} -> EL=:=[];
	Pointer ->
	    {list_item, _ , D } = nifty:dereference(nifty:as_type(Pointer, c_list, "struct list_item *")),
	    nifty:free(Pointer),
	    [H|_] = EL,
	    H=:=D
    end;
postcondition(S, {call, _, list_chop, [L]}, Result) ->
    EL = dict:fetch(L, S#state.ref),
    case Result of	
	{0, _} -> EL=:=[];
	Pointer ->
	    {list_item, _ , D } = nifty:dereference(nifty:as_type(Pointer, c_list, "struct list_item *")),
	    nifty:free(Pointer),
	    T= lists:last(EL),
	    T=:=D
    end;
%% postcondition(S, {call, _, list_length, [L]},Result) ->
%%     length(dict:fetch(L, S#state.ref)) =:= Result; %% length of the reference implementation should be the same
postcondition(_,_,_) ->
    true.

prop_list_works_fine() ->
    begin
	setup(),
	?FORALL(Cmds, commands(?MODULE),
		?TRAPEXIT(
		   begin
		       {History,State,Result} = run_commands(?MODULE, Cmds),
		       true,
		       ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
					   [History,State,Result]),
				 Result =:= ok)
		   end))
    end.

test() ->
    proper:quickcheck(prop_list_works_fine(), 1000).
