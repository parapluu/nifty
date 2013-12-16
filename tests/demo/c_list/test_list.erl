-module(test_list).
-export([test/0]).


new_item(Data) ->
	c_list:record_to_erlptr({list_item, null, Data}).

test() ->
	LIST = nifty:as_type(nifty:pointer(), c_list, "list_t"),
	ok = c_list:list_init(LIST),
	I1 = new_item(1),
	ok = c_list:list_add(LIST, I1),
	I2 = new_item(2),
	ok = c_list:list_add(LIST, I2),
	I3 = new_item(3),
	ok = c_list:list_add(LIST, I3),
	P1 = c_list:list_pop(LIST),
	{list_item, _, D1} = nifty:dereference(nifty:as_type(P1, c_list, "struct list_item *")),
	P2 = c_list:list_pop(LIST),
	{list_item, _, D2} = nifty:dereference(nifty:as_type(P2, c_list, "struct list_item *")),
	P3 = c_list:list_pop(LIST),
	{list_item, _, D3} = nifty:dereference(nifty:as_type(P3, c_list, "struct list_item *")),
	io:format("list: ~p ~n", [[D1, D2, D3]]),
	nifty:free(P1),
	nifty:free(P2),
	nifty:free(P3).
