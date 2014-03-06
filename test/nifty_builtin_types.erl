-module(nifty_builtin_types).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% this might be weird:
%%     rebar copies all erlang files into .eunit/
%%     in order to get to the c files we have adjust the path with ../test/
compile_module() ->
    ok = nifty_compiler:compile("../test/cfiles/builtin_types.h", nt_builtin, 
    			   [
    			   {port_specs,
    			     [{
    						".*",
    						"priv/nt_builtin_nif.so",	
    						["../test/cfiles/builtin_types.c"]
    				}]
    		}]).

call_functions()->
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
    ok.

builtin_test()->
    ok = compile_module(),
    ok = call_functions().
