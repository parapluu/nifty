%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_utils).
-export([expand/1,
	 new_config/0,
	 add_sources/2,
	 add_cflags/2,
	 add_ldflags/2,
	 merge_nif_spec/2]).


-type env() :: {'env', [{nonempty_string(), string()}]}.
-type target_options() :: [env()].
-type arch() :: nonempty_string().
-type target_name() :: nonempty_string().
-type source() :: nonempty_string().
-type target() :: {arch(), target_name(), [source()]} | {arch(), target_name(), [source()], target_options()}.
-type port_specs() :: {'port_specs', [target()]}.
-type config() :: [port_specs() | {atom(), term()}].

%% @doc Merges two configurations
-spec merge_nif_spec(config(), target()) -> config().
merge_nif_spec(Config, {".*", "$NIF", Sources, [{env, Env}]} = Spec) ->
    case proplists:get_value(port_specs, Config) of
	undefined -> 
	    [{port_specs, [Spec]} | Config];
	Specs ->
	    NewSpecs = case get_nifspec(Specs) of
			   {".*", "$NIF", OldSources} ->
			       store_nifspec({".*", "$NIF", OldSources ++ Sources, [{env, Env}]}, Specs);
			   {".*", "$NIF", OldSources, [{env, OldEnv}]} ->
			       store_nifspec({".*", "$NIF", OldSources ++ Sources, [{env, OldEnv ++ Env}]}, Specs);
			   undefined ->
			       store_nifspec({".*", "$NIF", Sources, [{env, []}] }, Specs)
		       end,
	    [{port_specs, NewSpecs} | proplists:delete(port_specs, Config)]
    end.

get_nifspec([]) -> undefined;
get_nifspec([H|T]) ->
    case H of
	{".*", "$NIF", _} -> H;
	{".*", "$NIF", _, _} -> H;
	_ -> get_nifspec(T)
    end.

store_nifspec(NifSpec, Specs) ->
    store_nifspec(Specs, NifSpec, []).

store_nifspec([], _, Acc) -> Acc;
store_nifspec([H|T], Spec, Acc) -> 
    case H of
	{".*", "$NIF", _} -> store_nifspec(T, Spec, [Spec|Acc]);
	{".*", "$NIF", _, _} -> store_nifspec(T, Spec, [Spec|Acc]);
	_ -> store_nifspec(T, Spec, [H|Acc])
    end.

%% @doc Returns an empty configuration
-spec new_config() -> [].
new_config() -> [].

%% @doc Adds the sources <code>S</code> to the NIF module
-spec add_sources([source()], config()) -> config().
add_sources(S, C) ->
    merge_nif_spec(C, {".*", "$NIF", S, [{env, []}]}).

%% @doc Adds the compile flags <code>F</code> to the NIF module
-spec add_cflags(string(), config()) -> config().
add_cflags(F, C) ->
    merge_nif_spec(C, {".*", "$NIF", [], [{env, [{"CFLAGS", "$CFLAGS "++F}]}]}).

%% @doc Adds link flags <code>F</code> to the NIF module
-spec add_ldflags(string(), config()) -> config().
add_ldflags(F, C) ->
    merge_nif_spec(C, {".*", "$NIF", [], [{env, [{"LDFLAGS", "$LDFLAGS "++F}]}]}).

%% @doc Returns <code>Path</code> with all environment variables
%% expanded
-spec expand(string()) -> string().
expand(Path) ->
    string:strip(lists:foldr(fun(A, Acc) -> A++" "++Acc end,
			     [],
			     tokenize(Path))).

%% copied from getopts 
-define(IS_WHITESPACE(Char), ((Char) =:= $\s orelse (Char) =:= $\t orelse (Char) =:= $\n orelse (Char) =:= $\r)).
-spec tokenize(CmdLine :: string()) -> [nonempty_string()].
tokenize(CmdLine) ->
    tokenize(CmdLine, [], []).

-spec tokenize(CmdLine :: string(), Acc :: [string()], ArgAcc :: string()) -> [string()].
tokenize([Sep | Tail], Acc, ArgAcc) when ?IS_WHITESPACE(Sep) ->
    NewAcc = case ArgAcc of
                 [_ | _] ->
                     %% Found separator: add to the list of arguments.
                     [lists:reverse(ArgAcc) | Acc];
                 [] ->
                     %% Found separator with no accumulated argument; discard it.
                     Acc
             end,
    tokenize(Tail, NewAcc, []);
tokenize([QuotationMark | Tail], Acc, ArgAcc) when QuotationMark =:= $"; QuotationMark =:= $' ->
    %% Quoted argument (might contain spaces, tabs, etc.)
    tokenize_quoted_arg(QuotationMark, Tail, Acc, ArgAcc);
tokenize([Char | _Tail] = CmdLine, Acc, ArgAcc) when Char =:= $$; Char =:= $% ->
    %% Unix and Windows environment variable expansion: ${VAR}; $VAR; %VAR%
    {NewCmdLine, Var} = expand_env_var(CmdLine),
    tokenize(NewCmdLine, Acc, lists:reverse(Var, ArgAcc));
tokenize([$\\, Char | Tail], Acc, ArgAcc) ->
    %% Escaped char.
    tokenize(Tail, Acc, [Char | ArgAcc]);
tokenize([Char | Tail], Acc, ArgAcc) ->
    tokenize(Tail, Acc, [Char | ArgAcc]);
tokenize([], Acc, []) ->
    lists:reverse(Acc);
tokenize([], Acc, ArgAcc) ->
    lists:reverse([lists:reverse(ArgAcc) | Acc]).

-spec tokenize_quoted_arg(QuotationMark :: char(), CmdLine :: string(), Acc :: [string()], ArgAcc :: string()) -> [string()].
tokenize_quoted_arg(QuotationMark, [QuotationMark | Tail], Acc, ArgAcc) ->
    %% End of quoted argument
    tokenize(Tail, Acc, ArgAcc);
tokenize_quoted_arg(QuotationMark, [$\\, Char | Tail], Acc, ArgAcc) ->
    %% Escaped char.
    tokenize_quoted_arg(QuotationMark, Tail, Acc, [Char | ArgAcc]);
tokenize_quoted_arg($" = QuotationMark, [Char | _Tail] = CmdLine, Acc, ArgAcc) when Char =:= $$; Char =:= $% ->
    %% Unix and Windows environment variable expansion (only for double-quoted arguments): ${VAR}; $VAR; %VAR%
    {NewCmdLine, Var} = expand_env_var(CmdLine),
    tokenize_quoted_arg(QuotationMark, NewCmdLine, Acc, lists:reverse(Var, ArgAcc));
tokenize_quoted_arg(QuotationMark, [Char | Tail], Acc, ArgAcc) ->
    tokenize_quoted_arg(QuotationMark, Tail, Acc, [Char | ArgAcc]);
tokenize_quoted_arg(_QuotationMark, CmdLine, Acc, ArgAcc) ->
    tokenize(CmdLine, Acc, ArgAcc).

-spec expand_env_var(CmdLine :: nonempty_string()) -> {string(), string()}.
expand_env_var(CmdLine) ->
    case CmdLine of
        "${" ++ Tail ->
            expand_env_var("${", $}, Tail, []);
        "$" ++ Tail ->
            expand_env_var("$", Tail, []);
        "%" ++ Tail ->
            expand_env_var("%", $%, Tail, [])
    end.

-spec expand_env_var(Prefix :: string(), EndMark :: char(), CmdLine :: string(), Acc :: string()) -> {string(), string()}.
expand_env_var(Prefix, EndMark, [Char | Tail], Acc)
  when (Char >= $A andalso Char =< $Z) orelse (Char >= $a andalso Char =< $z) orelse
       (Char >= $0 andalso Char =< $9) orelse (Char =:= $_) ->
    expand_env_var(Prefix, EndMark, Tail, [Char | Acc]);
expand_env_var(Prefix, EndMark, [EndMark | Tail], Acc) ->
    {Tail, get_env_var(Prefix, [EndMark], Acc)};
expand_env_var(Prefix, _EndMark, CmdLine, Acc) ->
    {CmdLine, Prefix ++ lists:reverse(Acc)}.


-spec expand_env_var(Prefix :: string(), CmdLine :: string(), Acc :: string()) -> {string(), string()}.
expand_env_var(Prefix, [Char | Tail], Acc)
  when (Char >= $A andalso Char =< $Z) orelse (Char >= $a andalso Char =< $z) orelse
       (Char >= $0 andalso Char =< $9) orelse (Char =:= $_) ->
    expand_env_var(Prefix, Tail, [Char | Acc]);
expand_env_var(Prefix, CmdLine, Acc) ->
    {CmdLine, get_env_var(Prefix, "", Acc)}.


-spec get_env_var(Prefix :: string(), Suffix :: string(), Acc :: string()) -> string().
get_env_var(Prefix, Suffix, [_ | _] = Acc) ->
    Name = lists:reverse(Acc),
    %% Only expand valid/existing variables.
    case os:getenv(Name) of
        false -> Prefix ++ Name ++ Suffix;
        Value -> Value
    end;
get_env_var(Prefix, Suffix, []) ->
    Prefix ++ Suffix.
