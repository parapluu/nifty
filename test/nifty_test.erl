-module(nifty_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

compile_builtin() ->
    ok = nifty_compiler:compile("../test/cfiles/builtin_types.h", nt_builtin, 
    			   [
    			   {port_specs,
    			     [{
    						".*",
    						"$NIF",
    						["../test/cfiles/builtin_types.c"]
    				}]
    		}]).

call_functions_builtin()->
    1 = nt_builtin:f1(1),
    1 = nt_builtin:f2(1),
    1 = nt_builtin:f3(1),
    1 = nt_builtin:f4(1),
    1 = nt_builtin:f5(1),
    1 = nt_builtin:f6(1),
    1 = nt_builtin:f7(1),
    1 = nt_builtin:f8(1),
    1.0 = nt_builtin:f9(1.0),
    1.0 = nt_builtin:f10(1.0),
    P = nifty:pointer(),
    nt_builtin:f11(P),
    nifty:free(P),
    P2 = nifty:mem_alloc(10),
    nt_builtin:f11(P2),
    nifty:free(P2),
    ok.

builtin_test()->
    ok = compile_builtin(),
    ok = call_functions_builtin().

compile_arguments() ->
    ok = nifty_compiler:compile("../test/cfiles/arguments.h", 
				nt_arguments, 
				[{port_specs,
				  [{".*",
				    "$NIF",	
				    ["../test/cfiles/arguments.c"]
				   }]
				 }]).

call_functions_arguments()->
    ok = nt_arguments:f1(),
    0 = nt_arguments:f3(0,0,0,0),
    ok = nt_arguments:f2(),
    1 = nt_arguments:f3(0,0,0,0),
    10 = nt_arguments:f4(1,2,3,4),
    ok.


arguments_test()->
    ok = compile_arguments(),
    ok = call_functions_arguments().

compile_structs() ->
    ok = nifty_compiler:compile("../test/cfiles/structs.h", nt_structs, []).

call_functions_structs()->
    ok.

structs_test()->
    ok = compile_structs(),
    ok = call_functions_structs().

compile_proxy() ->
    ok = nifty_compiler:compile("../test/cfiles/proxy_header.h", 
				nt_proxy, 
				[{port_specs,
				  [{".*",
				    "$NIF",	
				    ["../test/cfiles/proxy_header.c"],
				    [{env, [{"CFLAGS", "$CFLAGS -I../test/cfiles"}]}]
				   }]
				 }]).
call_functions_proxy() ->
    F = {0, "nifty.void*"},
    {0, _} = nt_proxy:f1(F),
    ok.

proxy_test()->
    ok = compile_proxy(),
    ok = call_functions_proxy().
    
