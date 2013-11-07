-module(test).
-export([main/1]).

test_header(Header, Module) ->
	io:format("processing ~s -> ~s ~s ~n", [Header, Module++"_nif.c", Module++".erl"]),
	% c parse stuff
	PathToH = Header,
	{Token, _} = clang_parse:parse([PathToH]),
	{Functions, Structs, Typedefs} = clang_parse:build_vars(Token),
	% template stuff
	CTemplate = erlang:list_to_atom("C"++Header),
	ETemplate = erlang:list_to_atom("E"++Header),
	ok = erlydtl:compile("templates/cmodule.tpl", CTemplate),
	ok = erlydtl:compile("templates/emodule.tpl", ETemplate),
	RenderVars = [
		{"functions", dict:to_list(Functions)},
		{"structs", dict:to_list(Structs)},
		{"typedefs", dict:to_list(Typedefs)},
		{"module", Module},
		{"header", Header}
		],
	{ok, COutput} = CTemplate:render(RenderVars),
 	{ok, EOutput} = ETemplate:render(RenderVars),
	ok = file:write_file(Module++"_nif.c", io_lib:fwrite("~s", [COutput])),
	ok = file:write_file(Module++".erl", io_lib:fwrite("~s", [EOutput])),
	ok.

main([Header, Module]) ->
	true = code:add_pathz(filename:dirname(escript:script_name())++ "/src"),
	test_header(Header, Module);
main(_) ->
	io:format("Error~n").
