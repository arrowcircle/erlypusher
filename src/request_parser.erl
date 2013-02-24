-module(request_parser).

-export([get_channel_name/1, get_action_name/1, get_pid_from_req/1]).

get_channel_name(Data) ->
  {[_|[{<<"data">>, {[{<<"channel">>, Channel_name}|_]}}|_]]} = jiffy:decode(Data),
  Channel_name.

get_action_name(Data) ->
  {[{_, ActionName}|_]} = jiffy:decode(Data),
  ActionName.

get_pid_from_req(Req) ->
  element(5, Req).