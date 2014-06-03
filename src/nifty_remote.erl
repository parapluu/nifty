%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
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
	 mem_write/1,
	 mem_write/2,
	 mem_read/2,
	 mem_alloc/1,
	 free/1,
	 %% configuration
	 get_config/0,
	 get_env/0,
	 %% builtin types
	 get_types/0
	]).

-spec start() -> ok.
start() ->
    nifty_remotecall:start().

-spec stop() -> ok.
stop() ->
    nifty_remotecall:stop().

-spec compile (term(),term(),term()) -> term().
compile(X0,X1,X2) ->
	nifty_remotecall:call_remote(nifty, compile, [X0,X1,X2]).

-spec list_to_cstr (term()) -> term().
list_to_cstr(X0) ->
	nifty_remotecall:call_remote(nifty, list_to_cstr, [X0]).

-spec cstr_to_list (term()) -> term().
cstr_to_list(X0) ->
	nifty_remotecall:call_remote(nifty, cstr_to_list, [X0]).

-spec dereference (term()) -> term().
dereference(X0) ->
	nifty_remotecall:call_remote(nifty, dereference, [X0]).

-spec pointer () -> term().
pointer() ->
	nifty_remotecall:call_remote(nifty, pointer, []).

-spec pointer (term()) -> term().
pointer(X0) ->
	nifty_remotecall:call_remote(nifty, pointer, [X0]).

-spec pointer_of (term(),term()) -> term().
pointer_of(X0,X1) ->
	nifty_remotecall:call_remote(nifty, pointer_of, [X0,X1]).

-spec pointer_of (term()) -> term().
pointer_of(X0) ->
	nifty_remotecall:call_remote(nifty, pointer_of, [X0]).

-spec as_type (term(),term()) -> term().
as_type(X0,X1) ->
	nifty_remotecall:call_remote(nifty, as_type, [X0,X1]).

-spec size_of (term()) -> term().
size_of(X0) ->
	nifty_remotecall:call_remote(nifty, size_of, [X0]).

-spec mem_write (term()) -> term().
mem_write(X0) ->
	nifty_remotecall:call_remote(nifty, mem_write, [X0]).

-spec mem_write (term(),term()) -> term().
mem_write(X0,X1) ->
	nifty_remotecall:call_remote(nifty, mem_write, [X0,X1]).

-spec mem_read (term(),term()) -> term().
mem_read(X0,X1) ->
	nifty_remotecall:call_remote(nifty, mem_read, [X0,X1]).

-spec mem_alloc (term()) -> term().
mem_alloc(X0) ->
	nifty_remotecall:call_remote(nifty, mem_alloc, [X0]).

-spec free (term()) -> term().
free(X0) ->
	nifty_remotecall:call_remote(nifty, free, [X0]).

-spec get_config () -> term().
get_config() ->
	nifty_remotecall:call_remote(nifty, get_config, []).

-spec get_env () -> term().
get_env() ->
	nifty_remotecall:call_remote(nifty, get_env, []).

-spec get_types () -> term().
get_types() ->
	nifty_remotecall:call_remote(nifty, get_types, []).

