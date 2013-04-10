-module(request_parser).

-export([get_channel_name/1, get_action_name/1, get_pid_from_req/1, get_auth/1]).

% {"event": "pusher:subscribe", "data ": {"channel": "private-MY_CHANNEL", "auth": "key1:33de107cea268ca8df11f1e42e2f2e590d52706e4907606531175a8516532c58"}}

get_channel_name(Data) ->
  {[_|[{<<"data">>, {[{<<"channel">>, Channel_name}|_]}}|_]]} = jiffy:decode(Data),
  Channel_name.

get_action_name(Data) ->
  {[{_, ActionName}|_]} = jiffy:decode(Data),
  ActionName.

get_auth(Data) ->
  {[_|[{<<"data">>, {[_|[{<<"auth">>, Auth}|_]]}}|_]]} = jiffy:decode(Data),
  Auth.

get_pid_from_req(Req) ->
  element(5, Req).