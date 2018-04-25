%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2014-2016, Andreas Löscher <andreas.loscher@it.uu.se>
%%%                      and Konstantinos Sagonas <kostis@it.uu.se>
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%% -------------------------------------------------------------------

-module(nifty_remotecall).

-export([start/0,
         stop/0,
         restart/0,
         slave_server/1,
         call_remote/3]).


%% @doc Starts remote node to safely call NIFs.
-spec start() -> ok.
start() ->
  %% try to start epmd
  [] = os:cmd("epmd -daemon"),
  Host = list_to_atom(net_adm:localhost()),
  case net_kernel:start([mastername(), shortnames]) of
    {ok, Pid} ->
      Pid;
    {error, {{already_started, Pid},_}} ->
      Pid;
    {error, {already_started, Pid}} ->
      Pid
  end,
  F =fun(Str,Binding) ->
    {ok,Ts,_} = erl_scan:string(Str),
    Ts1 = case lists:reverse(Ts) of
              [{dot,_}|_] -> Ts;
              TsR -> lists:reverse([{dot,1} | TsR])
          end,
    {ok,Expr} = erl_parse:parse_exprs(Ts1),
    erl_eval:exprs(Expr, Binding) end,
  {_,FUN,_} = F("fun() ->
      receive
        stop ->
          ok;
        {P, Paths} ->
          lists:foreach(fun code:add_patha/1, Paths),
          nifty_remotecall:slave_server(P)
      end
  end",[]),
  case slave:start_link(Host, slavename()) of
    {ok, Node} ->
      SlavePid = spawn(Node, FUN),
      SlavePid ! {self(), code:get_path()},
      undefined = put(slave_pid, SlavePid),
      ok;
    {error, {already_running, _}} ->
      ok
  end.

%% @doc Stops remote node
-spec stop() -> ok.
stop() ->
  case is_slave_alive() of
    true ->
      Host = list_to_atom(net_adm:localhost()),
      erase(slave_pid),
      {error, {already_running, Node}} = slave:start_link(Host, slavename()),
      slave:stop(Node);
    false ->
      ok
  end,
  _ = net_kernel:stop(), %% after this call the kernel is not running
  ok.

%% @doc Restarts remote node, useful for testing (side-effects are reset).
-spec restart() -> ok.
restart() ->
  stop(),
  start().

slave_server(P) ->
  receive
    stop ->
      ok;
    alive ->
      P ! ok,
      slave_server(P);
    {Module, Function, Args} ->
      RetMsg = try erlang:apply(Module, Function, Args) of
                   RetVal ->
                   {return, RetVal}
               catch
                 throw:Error ->
                   {throw, Error};
                 error:Error ->
                   {error, Error};
                 exit:Error ->
                   {exit, Error}
               end,
      P ! RetMsg,
      slave_server(P)
  end.

%% @doc Works like erlang:apply/3, with the exception that the calls
%% are redirected to a remote node. The node must be started in order
%% to be able to call functions remotely.  If the node is down,
%% <code>{error, node_down}</code> is thrown. If the node crashes
%% during the call (SIGSEGV or similar) <code>{error,
%% node_crashed}</code> is thrown.
-spec call_remote(atom(), atom(), [term()]) -> term().
call_remote(Mod, Func, Args) ->
  case get(slave_pid) of
    undefined ->
      {error, node_down};
    Slave ->
      Slave ! {Mod, Func, Args},
      receive_msg(100)
  end.

receive_msg(T) ->
  receive
    {return, RetVal} ->
      RetVal;
    {throw, Error} ->
      throw(Error);
    {error, Error} ->
      erlang:error(Error);
    {exit, Error} ->
      erlang:exit(Error)
  after T ->
      case is_slave_alive() of
        false ->
          erlang:error(node_crashed);
        true ->
          receive_msg(1000)
      end
  end.

is_slave_alive() ->
  Host = list_to_atom(net_adm:localhost()),
  case slave:start_link(Host, slavename()) of
    {ok, Node} ->
      slave:stop(Node),
      erase(slave_pid),
      false;
    {error, {already_running, _}} ->
      true
  end.

mastername() ->
  list_to_atom(processname()++"_master").

slavename() ->
  list_to_atom(processname()++"_slave").

processname() ->
  I = pid_to_list(self()),
  [_, PN1, PN2] = string:tokens(I, "<.>"),
  "p"++PN1++"_p"++PN2.
