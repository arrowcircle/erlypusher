-module(request_parser).

-export([get_channel_name/1, get_action_name/1, get_pid_from_req/1]).

get_channel_name(Data) ->
  {struct, [_|[{<<"data">>, {struct, [{<<"channel">>, Channel_name}|_]}}|_]]} = mochijson2:decode(binary_to_list(Data)),
  Channel_name.

get_action_name(Data) ->
  {struct, [{_, ActionName}|_]} = mochijson2:decode(binary_to_list(Data)),
  ActionName.

get_pid_from_req(Req) ->
  element(5, Req).