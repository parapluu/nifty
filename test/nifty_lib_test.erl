-module(nifty_lib_test).
-export([cstr_list_test/0,
	 read_write_test/0]).

-include_lib("proper/include/proper.hrl").
-inlcude_lib("eunit/include/eunit.hrl").

-type mystring() :: [1..255].
-type byte() :: 0..255.

%% auxilary functions
-spec cstr_list_comp(mystring()) -> boolean().
cstr_list_comp(Str) ->
    CStr = nifty:list_to_cstr(Str),
    Ret = nifty:dereference(CStr) =:= Str,
    ok = nifty:free(CStr),
    Ret.

-spec write_read([byte()]) -> boolean().
write_read(Data) ->
    Ptr = nifty:mem_write(Data),
    Ret = nifty:mem_read(Ptr, length(Data)) =:= Data,
    ok = nifty:free(Ptr),
    Ret.

%% properties
-spec prop_cstr_list() -> proper:outer_test().
prop_cstr_list() ->
    ?FORALL(S, mystring(), cstr_list_comp(S)).

-spec prop_read_write() -> proper:outer_test().
prop_read_write() ->
    ?FORALL(S, list(byte()), write_read(S)).

%% testcases
-spec cstr_list_test() -> boolean().
cstr_list_test() ->
    proper:quickcheck(prop_cstr_list(), [{to_file, user}, {numtests, 1000}]).

-spec read_write_test() -> boolean().
read_write_test() ->
    proper:quickcheck(prop_read_write(), [{to_file, user}, {numtests, 1000}]).
