-module(server_util).

-export([start/2, stop/1]).

start(ServerName, {Module, Function, Args}) ->
  global:trans({ServerName, ServerName},
    fun() ->
      case global:whereis_name(ServerName) of
        undefined ->
          Pid = spawn(Module, Function, Args),
          global:register_name(ServerName, Pid);
        _ ->
          io:format("Service ~p already started\n", [ServerName]),
          ok
      end
    end).

stop(ServerName) ->
  global:trans({ServerName, ServerName},
    fun() ->
      case global:whereis_name(ServerName) of
        undefined ->
          ok;
        _ ->
          global:send(ServerName, shutdown)
      end
    end).
