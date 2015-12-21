%%% -------------------------------------------------------------------
%%% Copyright (c) 2015, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% supress warning for test/0
-spec test() -> term().

-define(OPTS(F), [{port_specs, [{".*", "$NIF", [F]}]}]).

-spec compile_builtin() -> ok.
compile_builtin() ->
    ?_assertEqual(ok, nifty_compiler:compile("../test/cfiles/builtin_types.h",
					     nt_builtin,
					     ?OPTS("../test/cfiles/builtin_types.c"))).

-spec call_functions_builtin() -> term().
call_functions_builtin() ->
    [?_assertEqual(1, nt_builtin:f1(1)),
     ?_assertEqual(1, nt_builtin:f2(1)),
     ?_assertEqual(1, nt_builtin:f3(1)),
     ?_assertEqual(1, nt_builtin:f4(1)),
     ?_assertEqual(1, nt_builtin:f5(1)),
     ?_assertEqual(1, nt_builtin:f6(1)),
     ?_assertEqual(1, nt_builtin:f7(1)),
     ?_assertEqual(1, nt_builtin:f8(1)),
     ?_assertEqual(1.0, nt_builtin:f9(1.0)),
     ?_assertEqual(1.0, nt_builtin:f10(1.0)),
     ?_assertEqual({0, "nt_builtin.void *"}, nt_builtin:f11({0, "void *"})),
     ?_assertEqual(ok, fun () ->
			       P = nifty:mem_alloc(10),
			       {_, _} = nt_builtin:f12(P),
			       nifty:free(P)
		       end())].

-spec builtin_test_() -> term().
builtin_test_() ->
    {timeout, 180, [compile_builtin(),
		   call_functions_builtin()]}.

%% -spec call_functions_builtin_remote() -> ok.
%% call_functions_builtin_remote() ->
%%     1 = nt_builtin_remote:f1(1),
%%     1 = nt_builtin_remote:f2(1),
%%     1 = nt_builtin_remote:f3(1),
%%     1 = nt_builtin_remote:f4(1),
%%     1 = nt_builtin_remote:f5(1),
%%     1 = nt_builtin_remote:f6(1),
%%     1 = nt_builtin_remote:f7(1),
%%     1 = nt_builtin_remote:f8(1),
%%     1.0 = nt_builtin_remote:f9(1.0),
%%     1.0 = nt_builtin_remote:f10(1.0),
%%     P = nifty:pointer(),
%%     nt_builtin_remote:f11(P),
%%     nifty:free(P),
%%     P2 = nifty:mem_alloc(10),
%%     nt_builtin_remote:f11(P2),
%%     nifty:free(P2),
%%     ok.

%% -spec builtin_remote_test() -> ok.
%% builtin_remote_test() ->
%%     ok = nt_builtin_remote:start(),
%%     ok = call_functions_builtin_remote(),
%%     ok = nt_builtin_remote:stop().

-spec compile_arguments() -> term().
compile_arguments() ->
    ?_assertEqual(ok, nifty_compiler:compile("../test/cfiles/arguments.h",
					     nt_arguments,
					     ?OPTS("../test/cfiles/arguments.c"))).

-spec call_functions_arguments() -> term().
call_functions_arguments() ->
    [?_assertEqual(ok, nt_arguments:f1()),
     ?_assertEqual(0, nt_arguments:f3(0,0,0,0)),
     ?_assertEqual(ok, nt_arguments:f2()),
     ?_assertEqual(1, nt_arguments:f3(0,0,0,0)),
     ?_assertEqual(10, nt_arguments:f4(1,2,3,4))].

-spec arguments_test_() -> term().
arguments_test_()->
    {timeout, 180, [compile_arguments(),
		   call_functions_arguments()]}.

-spec compile_structs() -> term().
compile_structs() ->
    ?_assertEqual(ok, nifty_compiler:compile("../test/cfiles/structs.h", nt_structs, [])).

-spec  call_functions_structs() -> ok.
call_functions_structs() ->
    [?_assertMatch({_,_,_,_,_}, nifty:dereference(nt_structs:record_to_erlptr(nt_structs:new("struct s1")))),
     ?_assertMatch({_,_,_}, nifty:dereference(nt_structs:record_to_erlptr(nt_structs:new("struct s2")))),
     ?_assertMatch({_,_,_}, nifty:dereference(nt_structs:record_to_erlptr(nt_structs:new("struct s3")))),
     ?_assertMatch({_,_,_}, nifty:dereference(nt_structs:record_to_erlptr(nt_structs:new("struct s4")))),
     ?_assertEqual({s4, 0.5, 10}, nifty:dereference(nifty:pointer_of({s4, 0.5, 10}, "nt_structs.struct s4")))].

-spec structs_test_() -> term().
structs_test_() ->
    {timeout, 180, [compile_structs(),
		   call_functions_structs()]}.

-spec compile_proxy() -> term().
compile_proxy() ->
    ?_assertEqual(ok, nifty_compiler:compile("../test/cfiles/proxy_header.h",
					     nt_proxy,
					     nifty_utils:add_sources(
					       ["../test/cfiles/proxy_header.c"],
					       nifty_utils:add_cflags("-I../test/cfiles", [])))).

-spec call_functions_proxy() -> term().
call_functions_proxy() ->
    [?_assertMatch({0, _}, nt_proxy:fproxy({0, "void *"})),
     ?_assertEqual('none', proplists:lookup(strcmp, nt_proxy:module_info(exports)))].


-spec proxy_test_() -> term().
proxy_test_()->
    {timeout, 180, [compile_proxy(),
		   call_functions_proxy()]}.

-spec fptr_test_() -> term().
fptr_test_() ->
    {timeout, 180,
     ?_assertEqual(ok, nifty_compiler:compile("../test/cfiles/fptr.h", nt_fptr, []))}.

-spec compile_array() -> term().
compile_array() ->
    ?_assertEqual(ok, nifty_compiler:compile(
			"../test/cfiles/array.h", nt_array,
			nifty_utils:add_sources(["../test/cfiles/array.c"], []))).

-spec call_functions_array() -> ok.
call_functions_array() ->
    [?_assertEqual(10, nt_array:sumarray(nifty:mem_write([1,0,0,0,1,0,0,0,1,0,0,0,
							  1,0,0,0,1,0,0,0,1,0,0,0,
							  1,0,0,0,1,0,0,0,1,0,0,0,
							  1,0,0,0]))),
     ?_assertEqual(20, fun () ->
			       B = [1,1,1,1,1,1,1,1,1,1],
			       Rec = {array_st, nifty:mem_write(B), 0, nifty:mem_write(B)},
			       Ptr = nifty:pointer_of(Rec, "nt_array.struct array_st"),
			       nt_array:sumstruct_array(Ptr)
		       end())].

-spec array_test_() -> term().
array_test_() ->
    {timeout, 180, [compile_array(),
		   call_functions_array()]}.

-spec compile_tut2() -> term().
compile_tut2() ->
    ?_assertEqual(ok, nifty_compiler:compile("../test/cfiles/answer.h",
					     nt_tut2,
					     nifty_utils:add_sources(
					       ["../test/cfiles/answer.c"],
					       nifty_utils:add_cflags(
						 "-I../test/cfiles", [])))).

-spec call_tut2() -> term().
call_tut2() ->
    ?_assertEqual(42, nt_tut2:life_universe_and_everything()).

-spec tut2_test_() -> term().
tut2_test_()->
    {timeout, 180, [compile_tut2(),
		   call_tut2()]}.

-spec compile_dereference_regression() -> term().
compile_dereference_regression() ->
    ?_assertEqual(ok, nifty_compiler:compile("../test/cfiles/dereference_regression.h",
					     dereference_regression,
					     nifty_utils:add_sources(
					       ["../test/cfiles/dereference_regression.c"],
					       nifty_utils:add_cflags(
						 "-I../test/cfiles", [])))).

-spec call_dereference_regression() -> term().
call_dereference_regression() ->
    ?_assert(begin
		 P = nifty:pointer("dereference_regression.struct s"),
		 PP = nifty:pointer_of(P),
		 P == nifty:dereference(PP)
	     end).

-spec dereference_regression_test_() -> term().
dereference_regression_test_()->
    {timeout, 180, [compile_dereference_regression(),
		   call_dereference_regression()]}.


-spec enum_test_() -> term().
enum_test_() ->
    {timeout, 180,
     [compile_enum(),
      call_enum(),
      check_enum()]}.

compile_enum() ->
    ?_assertEqual(ok, nifty:compile("../test/cfiles/enums.h", nt_enums, [])).

call_enum() ->
    ?_assertEqual(1, nt_enums:f1(1,2)).

check_enum() ->
    ?_assertEqual(100, nifty:enum_value(nt_enums, "VALUE6")).
