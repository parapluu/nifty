-module(nifty_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% supress warning for test/0
-spec test() -> term().

-define(OPTS(F), [{port_specs, [{".*", "$NIF", [F]}]}]).

-spec compile_builtin() -> ok.
compile_builtin() ->
    ok = nifty_compiler:compile("../test/cfiles/builtin_types.h", 
				nt_builtin, 
				?OPTS("../test/cfiles/builtin_types.c")).

-spec call_functions_builtin() -> ok.
call_functions_builtin() ->
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

-spec builtin_test() -> ok.
builtin_test() ->
    ok = compile_builtin(),
    ok = call_functions_builtin().


-spec call_functions_builtin_remote() -> ok.
call_functions_builtin_remote() ->
    1 = nt_builtin_remote:f1(1),
    1 = nt_builtin_remote:f2(1),
    1 = nt_builtin_remote:f3(1),
    1 = nt_builtin_remote:f4(1),
    1 = nt_builtin_remote:f5(1),
    1 = nt_builtin_remote:f6(1),
    1 = nt_builtin_remote:f7(1),
    1 = nt_builtin_remote:f8(1),
    1.0 = nt_builtin_remote:f9(1.0),
    1.0 = nt_builtin_remote:f10(1.0),
    P = nifty:pointer(),
    nt_builtin_remote:f11(P),
    nifty:free(P),
    P2 = nifty:mem_alloc(10),
    nt_builtin_remote:f11(P2),
    nifty:free(P2),
    ok.

-spec builtin_remote_test() -> ok.
builtin_remote_test() ->
    ok = nt_builtin_remote:start(),
    ok = call_functions_builtin_remote(),
    ok = nt_builtin_remote:stop().

-spec compile_arguments() -> ok.
compile_arguments() ->
    ok = nifty_compiler:compile("../test/cfiles/arguments.h", 
				nt_arguments, 
				?OPTS("../test/cfiles/arguments.c")).

-spec call_functions_arguments() -> ok.
call_functions_arguments() ->
    ok = nt_arguments:f1(),
    0 = nt_arguments:f3(0,0,0,0),
    ok = nt_arguments:f2(),
    1 = nt_arguments:f3(0,0,0,0),
    10 = nt_arguments:f4(1,2,3,4),
    ok.

-spec arguments_test() -> ok.
arguments_test()->
    ok = compile_arguments(),
    ok = call_functions_arguments().

-spec compile_structs() -> ok.
compile_structs() ->
    ok = nifty_compiler:compile("../test/cfiles/structs.h", nt_structs, []).

-spec  call_functions_structs() -> ok.
call_functions_structs() ->
    {_,_,_,_,_} =  nifty:dereference(nt_structs:record_to_erlptr(nt_structs:new("struct s1"))),
    {_,_,_} = nifty:dereference(nt_structs:record_to_erlptr(nt_structs:new("struct s2"))),
    {_,_,_} = nifty:dereference(nt_structs:record_to_erlptr(nt_structs:new("struct s3"))),
    {_,_,_} = nifty:dereference(nt_structs:record_to_erlptr(nt_structs:new("struct s4"))),
    S4 = {s4, 0.5, 10},
    S4 = nifty:dereference(nifty:pointer_of(S4, "nt_structs.struct s4")),
    ok.

-spec structs_test() -> ok.
structs_test() ->
    ok = compile_structs(),
    ok = call_functions_structs().

-spec compile_proxy() -> ok.
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

-spec call_functions_proxy() -> ok.
call_functions_proxy() ->
    F = {0, "nifty.void*"},
    {0, _} = nt_proxy:fproxy(F),
    none = proplists:lookup(strcmp, nt_proxy:module_info(exports)),
    ok.


-spec proxy_test() -> ok.
proxy_test()->
    ok = compile_proxy(),
    ok = call_functions_proxy().

-spec fptr_test() -> ok.
fptr_test() ->
    ok = nifty_compiler:compile("../test/cfiles/fptr.h", nt_fptr, []).

-spec compile_array() -> ok.
compile_array() ->
    ok = nifty_compiler:compile("../test/cfiles/array.h", 
				nt_array, 
				?OPTS("../test/cfiles/array.c")).
-spec call_functions_array() -> ok.
call_functions_array() ->
    A = [1,0,0,0,
	 1,0,0,0,
	 1,0,0,0,
	 1,0,0,0,
	 1,0,0,0,
	 1,0,0,0,
	 1,0,0,0,
	 1,0,0,0,
	 1,0,0,0,
	 1,0,0,0],
    10=nt_array:sumarray(nifty:mem_write(A)),
    %% struct
    B = [1,1,1,1,1,1,1,1,1,1],
    Rec = {array_st, nifty:mem_write(B), 0, nifty:mem_write(B)},
    Ptr = nifty:pointer_of(Rec, "nt_array.struct array_st"),
    20 = nt_array:sumstruct_array(Ptr),
    ok.

-spec array_test() -> ok.
array_test() ->
    ok = compile_array(),
    ok = call_functions_array().


-spec compile_tut2() -> ok.
compile_tut2() ->
    ok = nifty_compiler:compile("../test/cfiles/answer.h", 
				nt_tut2, 
				[{port_specs,
				  [{".*",
				    "$NIF",	
				    ["../test/cfiles/answer.c"],
				    [{env, [{"CFLAGS", "$CFLAGS -I../test/cfiles"}]}]
				   }]
				 }]).

-spec call_tut2() -> ok.
call_tut2() ->
    42=nt_tut2:life_universe_and_everything(),
    ok.


-spec tut2_test() -> ok.
tut2_test()->
    ok = compile_tut2(),
    ok = call_tut2().
