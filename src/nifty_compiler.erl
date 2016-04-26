%%% -------------------------------------------------------------------
%%% Copyright (c) 2014, Andreas Löscher <andreas.loscher@it.uu.se> and
%%%                     Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_compiler).
-export([render/4,
         compile/3]).

-type reason() :: atom().
-type options() :: proplists:proplist().
-type renderout() :: {iolist(), iolist(), iolist(), iolist(), iolist(), iolist()}.
-type modulename() :: string().

-export_type([options/0]).

%% @doc Renders an <code>InterfaceFile</code> into a Erlang module containing of <code>ModuleName</code>.erl
%% <code>ModuleName</code>.c, <code>ModuleName</code>.app and  <code>rebar</code>.config and returns the
%% contents of these files as tuple of iolists (in this order). It uses <code>CFlags</code> to parse the
%% <code>InterfaceFile</code> and <code>Options</code> to compile it. <code>Options</code> are equivalent to
%% rebar options.
-spec render(string(), modulename(), [string()], options()) -> {'error',reason()} | {renderout(), [nonempty_string()]}.
render(InterfaceFile, ModuleName, CFlags, Options) ->
    io:format("generating... ~n"),%%, [ModuleName, ModuleName++"_remote", InterfaceFile]),
    %% c parse stuff
    PathToH = InterfaceFile,
    case filelib:is_file(PathToH) andalso (not filelib:is_dir(PathToH)) of
        false ->
            {error, no_file};
        true ->
            case nifty_clangparse:parse([PathToH|CFlags]) of
                {error, fail} ->
                    {error, compile};
                {FuncLoc, Raw_Symbols, Raw_Types, Unsave_Constructors} ->
                    Constructors = check_constructors(Unsave_Constructors),
                    Unsave_Types = nifty_clangparse:build_type_table(Raw_Types, Constructors),
                    Types = check_types(Unsave_Types, Constructors),
                    Unsave_Symbols = filter_symbols(InterfaceFile, Raw_Symbols, FuncLoc),
                    {Symbols, Lost} = check_symbols(Unsave_Symbols, Types),
                    RenderVars = [{"module", ModuleName},
                                  {"header", InterfaceFile},
                                  {"config", Options},
                                  {"types", Types},
                                  {"symbols", Symbols},
                                  {"constructors", Constructors},
                                  {"none", none}],
                    COutput = render_with_errors(nifty_c_template, RenderVars),
                    ErlOutput = render_with_errors(nifty_erl_template, RenderVars),
                    SaveErlOutput = render_with_errors(nifty_save_erl_template, RenderVars),
                    HrlOutput = render_with_errors( nifty_hrl_template, RenderVars),
                    AppOutput = render_with_errors(nifty_app_template, RenderVars),
                    ConfigOutput = render_with_errors(nifty_config_template, RenderVars),
                    {{ErlOutput, SaveErlOutput, HrlOutput, COutput, AppOutput, ConfigOutput}, Lost}
            end
    end.

render_with_errors(Template, Vars) ->
    try Template:render(Vars) of
        {ok, Output} -> Output;
        {error, Err} ->
            io:format("Error during rendering of template ~p:~n~p~nPlease report the error~n", [Template, Err]),
            throw(nifty_render_error)
    catch
        ET:E ->
            io:format("~p:~p during rendering of temlate ~p:~nVars: ~n~p~nPlease report the error~n", [ET, E, Template, Vars]),
            throw(nifty_render_error)
    end.


check_types(Types, Constr) ->
    %% somehow we have incomplete types in the type table
    Pred = fun (Key, Value) ->
                   case Value of
                       {userdef, [{struct, Name}]} ->
                           dict:is_key({struct, Name}, Constr);
                       _ ->
                           nifty_types:check_type(Key, Types, Constr)
                   end
           end,
    dict:filter(Pred, Types).

check_constructors(Constr) ->
    Pred = fun (_, Fields) -> length(Fields)>0 end,
    dict:filter(Pred, Constr).

filter_symbols(InterfaceFile, Symbols, FuncLoc) ->
    BaseName = filename:basename(InterfaceFile),
    Pred = fun (Key, _) -> filename:basename(dict:fetch(Key, FuncLoc))=:=BaseName end,
    dict:filter(Pred, Symbols).

check_symbols(Symbols, Types) ->
    Pred = fun (_, Args) -> check_args(Args, Types) end,
    Accml = fun(Name, Args, AccIn) -> case check_args(Args, Types) of
                                          true -> AccIn;
                                          false -> [Name|AccIn]
                                      end
            end,
    {dict:filter(Pred, Symbols), dict:fold(Accml, [], Symbols)}.

check_args([], _) -> true;
check_args([H|T], Types) ->
    Type = case H of
               {return, Tp} -> Tp;
               {argument, _, Tp} -> Tp
           end,
    case nifty_types:check_type(Type, Types) of
        false ->
            false;
        true -> check_args(T, Types)
    end.

store_files(InterfaceFile, ModuleName, Options, RenderOutput) ->
    {ok, Path} = file:get_cwd(),
    store_files(InterfaceFile, ModuleName, Options, RenderOutput, Path).

store_files(_, ModuleName, _, RenderOutput, Path) ->
    ok = case file:make_dir(filename:join([Path,ModuleName])) of
             ok -> ok;
             {error,eexist} -> ok;
             _ -> fail
         end,
    ok = case file:make_dir(filename:join([Path,ModuleName, "src"])) of
             ok -> ok;
             {error,eexist} -> ok;
             _ -> fail
         end,
    ok = case file:make_dir(filename:join([Path,ModuleName, "include"])) of
             ok -> ok;
             {error,eexist} -> ok;
             _ -> fail
         end,
    ok = case file:make_dir(filename:join([Path,ModuleName, "c_src"])) of
             ok -> ok;
             {error,eexist} -> ok;
             _ -> fail
         end,
    ok = case file:make_dir(filename:join([Path,ModuleName, "ebin"])) of
             ok -> ok;
             {error,eexist} -> ok;
             _ -> fail
         end,
    {ErlOutput, SaveErlOutput, HrlOutput, COutput, AppOutput, ConfigOutput} = RenderOutput,
    ok = fwrite_render(Path, ModuleName, "src", ModuleName++".erl", ErlOutput),
    ok = fwrite_render(Path, ModuleName, "src", ModuleName++"_remote"++".erl", SaveErlOutput),
    ok = fwrite_render(Path, ModuleName, "include", ModuleName++".hrl", HrlOutput),
    ok = fwrite_render(Path, ModuleName, "c_src", ModuleName++"_nif.c", COutput),
    ok = fwrite_render(Path, ModuleName, "ebin", ModuleName++".app", AppOutput),
    ok = fwrite_render(Path, ModuleName, ".", "rebar.config", ConfigOutput).

fwrite_render(Path, ModuleName, Dir, FileName, Template) ->
    file:write_file(filename:join([Path, ModuleName, Dir, FileName]), [Template]).

compile_module(ModuleName) ->
    {ok, Path} = file:get_cwd(),
    ok = file:set_cwd(filename:join([Path, ModuleName])),
    try rebar_commands(["compile"]) of
        _ -> file:set_cwd(Path)
    catch
        throw:rebar_abort ->
            ok = file:set_cwd(Path),
            fail
    end.

rebar_commands(RawArgs) ->
    Args = nifty_rebar:parse_args(RawArgs),
    BaseConfig = nifty_rebar:init_config(Args),
    {BaseConfig1, Cmds} = nifty_rebar:save_options(BaseConfig, Args),
    nifty_rebar:run(BaseConfig1, Cmds).

%% @doc Generates a NIF module out of a C header file and compiles it,
%% generating wrapper functions for all functions present in the header file.
%% <code>InterfaceFile</code> specifies the header file. <code>Module</code> specifies
%% the module name of the translated NIF. <code>Options</code> specifies the compile
%% options. These options are a superset rebar's config options and include
%% additional Nifty options: <br/>
%%     <code>{nifty, NiftyOptions}</code> <br/>
%% where NiftyOptions is a list of options, which can be : <br/>
%% <table border="1">
%% <tr><td><code>schedule_dirty</code></td><td>use dirty schedulers</td></tr>
%% </table>
-spec compile(string(), module(), options()) -> 'ok' | {'error', reason()} | {'warning' , {'not_complete' , [nonempty_string()]}}.
compile(InterfaceFile, Module, Options) ->
    nifty_rebar:init(),
    ModuleName = atom_to_list(Module),
    os:putenv("NIF", libname(ModuleName)),
    {ok, NiftyRoot} = file:get_cwd(),
    os:putenv("NIFTY_ROOT", NiftyRoot),
    UCO = update_compile_options(InterfaceFile, ModuleName, Options),
    Env = build_env(ModuleName, UCO),
    CFlags = string:tokens(proplists:get_value("CFLAGS", Env, ""), " "),
    case render(InterfaceFile, ModuleName, CFlags, UCO) of
        {error, E} ->
            {error, E};
        {Output, Lost} ->
            ok = store_files(InterfaceFile, ModuleName, UCO, Output),
            case compile_module(ModuleName) of
                ok ->
                    ModulePath = filename:absname(filename:join([ModuleName, "ebin"])),
                    true = code:add_patha(ModulePath),
                    case Lost of
                        [] -> ok;
                        _ -> {warning, {not_complete, Lost}}
                    end;
                fail ->
                    {error, compile}
            end
    end.

build_env(ModuleName, Options) ->
    Env = case proplists:get_value(port_env, Options) of
              undefined -> [];
              EnvList -> EnvList
          end,
    EnvAll = case proplists:get_value(port_specs, Options) of
                 undefined -> Env;
                 SpecList ->
                     lists:concat([Env, get_spec_env(ModuleName, SpecList)])
             end,
    Config = rebar_config:set(rebar_config:new(), port_env, EnvAll),
    rebar_port_compiler:setup_env(Config).

get_spec_env(_, []) -> [];
get_spec_env(ModuleName, [S|T]) ->
    Lib = libname(ModuleName),
    case S of
        {_, Lib, _, Options} ->
            case proplists:get_value(env, Options) of
                undefined -> [];
                Env -> expand_env(Env, [])
            end;
        _ ->
            get_spec_env(ModuleName, T)
    end.

norm_opts(Options) ->
    case proplists:get_value(env, Options) of
        undefined -> Options;
        Env ->
            [{env, merge_env(expand_env(Env, []), dict:new())}| proplists:delete(env, Options)]
    end.

merge_env([], D) -> dict:to_list(D);
merge_env([{Key, Opt}|T], D) ->
    case dict:is_key(Key, D) of
        true ->
            merge_env(T, dict:store(Key, dict:fetch(Key,D) ++ " " ++ remove_envvar(Key, Opt), D));
        false ->
            merge_env(T, dict:store(Key, Opt, D))
    end.

remove_envvar(Key, Opt) ->
    %% remove in the beginning and the end
    Striped = string:strip(Opt),
    K1 = "${" ++ Key ++ "}",
    K2 = "$" ++ Key,
    K3 = "%" ++ Key,
    E1 = length(Striped) - length(K1) + 1,
    E23 = length(Striped) - length(K2) + 1,
    case string:str(Striped, K1) of
        1 ->
            string:substr(Striped, length(K1)+1);
        E1 ->
            string:substr(Striped, 1, E1);
        _ ->
            case string:str(Striped, K2) of
                1 ->
                    string:substr(Striped, length(K2)+1);
                E23 ->
                    string:substr(Striped, 1, E23 -1);
                _ ->
                    case string:str(Striped, K3) of
                        1 ->
                            string:substr(Striped, length(K3)+1);
                        E23 ->
                            string:substr(Striped, 1, E23 - 1);
                        _ ->
                            Striped
                    end
            end
    end.

expand_env([], Acc) ->
    Acc;
expand_env([{ON, O}|T], Acc) ->
    expand_env(T, [{ON, nifty_utils:expand(O)}|Acc]).

libname(ModuleName) ->
    "priv/"++ModuleName++"_nif.so".

update_compile_options(InterfaceFile, ModuleName, CompileOptions) ->
    NewPort_Spec = case proplists:get_value(port_specs, CompileOptions) of
                       undefined ->
                           [module_spec(".*", [], [], InterfaceFile, ModuleName)];
                       UPortSpec ->
                           update_port_spec(InterfaceFile, ModuleName, UPortSpec, [], false)
                   end,
    orddict:store(port_specs, NewPort_Spec, orddict:from_list(CompileOptions)).

module_spec(ARCH, Sources, Options, InterfaceFile,  ModuleName) ->
    {
      ARCH,
      libname(ModuleName),
      ["c_src/"++ModuleName++"_nif.c"|abspath_sources(Sources)],
      norm_opts(join_options([{env,
                               [{"CFLAGS",
                                 "$CFLAGS -I"++filename:absname(filename:dirname(nifty_utils:expand(InterfaceFile)))}]}],
                             Options))
    }.

join_options(Proplist1, Proplist2) ->
    orddict:merge(
      fun(_,X,Y) -> X++Y end,
      orddict:from_list(Proplist1),
      orddict:from_list(Proplist2)).

abspath_sources(S) -> abspath_sources(S, []).

abspath_sources([], Acc) -> Acc;
abspath_sources([S|T], Acc) ->
    abspath_sources(T, [filename:absname(nifty_utils:expand(S))|Acc]).


update_port_spec(_,  _, [], Acc, true) ->
    Acc;
update_port_spec(InterfaceFile,  ModuleName, [], Acc, false) -> %% empty spec
    [module_spec(".*", [], [], InterfaceFile, ModuleName), Acc];
update_port_spec(InterfaceFile,  ModuleName, [Spec|T], Acc, Found) ->
    Shared = libname(ModuleName),
    case expand_spec(Spec) of
        {ARCH, Shared, Sources} ->
            update_port_spec(
              InterfaceFile,
              ModuleName,
              T,
              [module_spec(ARCH, Sources, [], InterfaceFile, ModuleName)|Acc], true);
        {ARCH, Shared, Sources, Options} ->
            update_port_spec(
              InterfaceFile,
              ModuleName,
              T,
              [module_spec(ARCH, Sources, Options, InterfaceFile, ModuleName)|Acc], true);
        _ ->
            update_port_spec(InterfaceFile, ModuleName, T, [Spec|Acc], Found)
    end.

expand_spec(S) ->
    case S of
        {ARCH, Shared, Sources} ->
            {ARCH, nifty_utils:expand(Shared), norm_sources(Sources)};
        {ARCH, Shared, Sources, Options} ->
            {ARCH, nifty_utils:expand(Shared), norm_sources(Sources), Options}
    end.

norm_sources(S) ->
    [nifty_utils:expand(X) || X <- S].
