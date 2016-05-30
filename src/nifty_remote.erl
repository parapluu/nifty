%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2014-2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                      and Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_remote).

-export([%% control
         start/0,
         stop/0,
         %% create modules
         compile/3,
         %% strings
         list_to_cstr/1,
         cstr_to_list/1,
         %% pointers
         dereference/1,
         pointer/0,
         pointer/1,
         pointer_of/2,
         pointer_of/1,
         %% types
         as_type/2,
         size_of/1,
         %% memory allocation
         malloc/1,
         mem_write/1,
         mem_write/2,
         mem_read/2,
         mem_alloc/1,
         mem_copy/3,
         free/1,
         %% configuration
         get_config/0,
         get_env/0,
         %% builtin types
         get_types/0,
         %% array
         array_new/2,
         array_ith/2,
         array_element/2,
         array_set/3,
         array_to_list/2,
         list_to_array/2,
         %% enums
         enum_value/2
        ]).

-spec start() -> ok.
start() ->
  nifty_remotecall:start().

-spec stop() -> ok.
stop() ->
  nifty_remotecall:stop().

-spec compile(string(), module(), nifty:options()) -> nifty:comp_ret() | {error, node_down}.
compile(X0,X1,X2) ->
  nifty_remotecall:call_remote(nifty, compile, [X0,X1,X2]).

-spec list_to_cstr(string()) -> nifty:ptr() | {error, node_down}.
list_to_cstr(X0) ->
  nifty_remotecall:call_remote(nifty, list_to_cstr, [X0]).

-spec cstr_to_list(nifty:ptr()) -> string() | {error, node_down}.
cstr_to_list(X0) ->
  nifty_remotecall:call_remote(nifty, cstr_to_list, [X0]).

-spec dereference(nifty:ptr()) -> nifty:cvalue() | {error, node_down}.
dereference(X0) ->
  nifty_remotecall:call_remote(nifty, dereference, [X0]).

-spec pointer() -> nifty:ptr() | {error, node_down}.
pointer() ->
  nifty_remotecall:call_remote(nifty, pointer, []).

-spec pointer(nonempty_string()) -> nifty:ptr() | undef | {error, node_down}.
pointer(X0) ->
  nifty_remotecall:call_remote(nifty, pointer, [X0]).

-spec pointer_of(term(), string()) -> nifty:ptr() | undef | {error, node_down}.
pointer_of(X0,X1) ->
  nifty_remotecall:call_remote(nifty, pointer_of, [X0,X1]).

-spec pointer_of(nifty:ptr()) -> nifty:ptr() | undef | {error, node_down}.
pointer_of(X0) ->
  nifty_remotecall:call_remote(nifty, pointer_of, [X0]).

-spec as_type(nifty:ptr(), nonempty_string()) -> nifty:ptr() | undef | {error, node_down}.
as_type(X0,X1) ->
  nifty_remotecall:call_remote(nifty, as_type, [X0,X1]).

-spec size_of(nonempty_string()) -> integer() | undef | {error, node_down}.
size_of(X0) ->
  nifty_remotecall:call_remote(nifty, size_of, [X0]).

-spec mem_write(binary() | list()) -> nifty:ptr() | {error, node_down}.
mem_write(X0) ->
  nifty_remotecall:call_remote(nifty, mem_write, [X0]).

-spec mem_write(nifty:ptr(), binary() | list()) -> nifty:ptr() | {error, node_down}.
mem_write(X0,X1) ->
  nifty_remotecall:call_remote(nifty, mem_write, [X0,X1]).

-spec mem_read (term(),term()) -> term() | {error, node_down}.
mem_read(X0,X1) ->
  nifty_remotecall:call_remote(nifty, mem_read, [X0,X1]).

-spec mem_alloc(non_neg_integer()) -> nifty:ptr() | {error, node_down}.
mem_alloc(X0) ->
  nifty_remotecall:call_remote(nifty, mem_alloc, [X0]).

-spec free(nifty:ptr()) -> 'ok' | {error, node_down}.
free(X0) ->
  nifty_remotecall:call_remote(nifty, free, [X0]).

-spec get_config() -> proplists:proplist() | {error, node_down}.
get_config() ->
  nifty_remotecall:call_remote(nifty, get_config, []).

-spec get_env() -> {integer(), nonempty_string()} | {error, node_down}.
get_env() ->
  nifty_remotecall:call_remote(nifty, get_env, []).

-spec get_types() -> dict:dict() | {error, node_down}.
get_types() ->
  nifty_remotecall:call_remote(nifty, get_types, []).

-spec enum_value(atom(), nonempty_string() | atom()) -> integer() | undef.
enum_value(X0, X1) ->
  nifty_remotecall:call_remote(nifty, enum_value, [X0, X1]).

-spec mem_copy(nifty:ptr(), nifty:ptr(), non_neg_integer()) -> ok.
mem_copy(X0, X1, X2) ->
  nifty_remotecall:call_remote(nifty, mem_copy, [X0, X1, X2]).

-spec array_new(nonempty_string(), non_neg_integer()) -> nifty:ptr().
array_new(X0, X1) ->
  nifty_remotecall:call_remote(nifty, array_new, [X0, X1]).

-spec array_ith(nifty:ptr(), integer()) -> nifty:ptr().
array_ith(X0, X1) ->
  nifty_remotecall:call_remote(nifty, array_ith, [X0, X1]).

-spec array_element(nifty:ptr(), integer()) -> nifty:cvalue().
array_element(X0, X1) ->
  nifty_remotecall:call_remote(nifty, array_element, [X0, X1]).

-spec array_set(nifty:ptr(), term(), integer()) -> ok.
array_set(X0, X1, X2) ->
  nifty_remotecall:call_remote(nifty, array_set, [X0, X1, X2]).

-spec array_to_list(nifty:ptr(), non_neg_integer()) -> [nifty:cvalue()].
array_to_list(X0, X1) ->
  nifty_remotecall:call_remote(nifty, array_to_list, [X0, X1]).

-spec list_to_array(list(), nonempty_string()) -> nifty:ptr().
list_to_array(X0, X1) ->
  nifty_remotecall:call_remote(nifty, list_to_array, [X0, X1]).

-spec malloc(non_neg_integer()) -> nifty:ptr().
malloc(X0) ->
  mem_alloc(X0).
