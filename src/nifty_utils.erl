-module(nifty_utils).
-export([expand/1]).


expand(String) ->
    string:strip(lists:foldr(fun(A, Acc) ->
				     A++" "++Acc end,
			     [],
			     tokenize(String))).

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
