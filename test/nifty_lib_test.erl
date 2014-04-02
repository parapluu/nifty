-module(nifty_lib_test).
-export([cstr_list_test/0,
	 read_write_test/0,
	 short_test/0,
	 int_deref_test/0,
	 pointer_of_pointer_test/0]).

-include_lib("proper/include/proper.hrl").
-inlcude_lib("eunit/include/eunit.hrl").

-type mystring() :: [1..255].
-type byte() :: 0..255.


%% auxilary functions
-spec cstr_list_comp(mystring()) -> boolean().
cstr_list_comp(Str) ->
    CStr = nifty:list_to_cstr(Str),
    DStr = nifty:dereference(CStr),
    ok = nifty:free(CStr),
    DStr =:= Str.

short_comp(Sh) ->
    P = nifty:pointer_of(Sh, "unsigned short"),
    DSh = nifty:dereference(P),
    ok = nifty:free(P),
    Sh =:= DSh.

-spec write_read([byte()]) -> boolean().
write_read(Data) ->
    Ptr = nifty:mem_write(Data),
    DData = nifty:mem_read(Ptr, length(Data)),
    ok = nifty:free(Ptr),
    DData =:= Data.

%% properties
-spec prop_cstr_list() -> proper:outer_test().
prop_cstr_list() ->
    ?FORALL(S, mystring(), cstr_list_comp(S)).

-spec prop_read_write() -> proper:outer_test().
prop_read_write() ->
    ?FORALL(S, list(byte()), write_read(S)).

-spec prop_short() -> proper:outer_test().
prop_short() ->
     ?FORALL(S, 
	     integer(0,trunc(math:pow(2,16))-1),
	     short_comp(S)).

%% testcases
-spec cstr_list_test() -> boolean().
cstr_list_test() ->
    proper:quickcheck(prop_cstr_list(), [{to_file, user}, {numtests, 1000}]).

-spec read_write_test() -> boolean().
read_write_test() ->
    proper:quickcheck(prop_read_write(), [{to_file, user}, {numtests, 1000}]).

-spec short_test() -> boolean().
short_test() ->
    proper:quickcheck(prop_short(), [{to_file, user}, {numtests, 1000}]).

-spec int_deref_test() -> boolean().
int_deref_test() ->
    Ptr = nifty:mem_write([16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff, 16#ff]),
    -1 = nifty:dereference(nifty:as_type(Ptr, nifty, "signed char *")),
    -1 = nifty:dereference(nifty:as_type(Ptr, nifty, "short *")),
    -1 = nifty:dereference(nifty:as_type(Ptr, nifty, "int *")),
    -1 = nifty:dereference(nifty:as_type(Ptr, nifty, "long *")),
    -1 = nifty:dereference(nifty:as_type(Ptr, nifty, "long long *")),
    ok = nifty:free(Ptr),
    true.

-spec pointer_of_pointer_test() -> boolean().
pointer_of_pointer_test() ->
    10 =:= nifty:dereference(nifty:dereference(nifty:pointer_of(nifty:pointer_of(10, "int"), "nifty.int *"))).
