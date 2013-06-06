-module(channel).
-export([handle/1]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

% api
api(event, {Name, Data, ChannelName}) ->
  jiffy:encode({[{<<"event">>, Name},
                 {<<"data">>, Data},
                 {<<"channel">>, ChannelName}]}).

% client

handle(Dict) ->
  {ok, [Event]} = dict:find("event", Dict),
  handle(Event, Dict).

handle(<<"pusher:ping">>, Dict) ->
  ping();

handle(<<"pusher:unsubscribe">>, Dict) ->
  unsubscribe(Dict);

handle(<<"pusher:subscribe">>, Dict) ->
  {ok, [ChannelName]} = dict:find("channel", Dict),
  {ok, [{AppId, _Key, _Secret, _Name}]} = dict:find("app", Dict),
  gproc:reg({p, g, {AppId, ChannelName}}),
  success(ChannelName).

unsubscribe(Dict) ->
  {ok, [ChannelName]} = dict:find("channel", Dict),
  {ok, [Pid]} = dict:find("pid", Dict),
  {ok, [{AppId, _Key, _Secret, _Name}]} = dict:find("app", Dict),
  gproc:unreg({p, g, {AppId, ChannelName}}),
  success(ChannelName).

init({ok, SocketId}) ->
  "{\"event\": \"pusher:connection_established\", \"data\": {\"socket_id\": \"" ++ SocketId ++ "\"}}";

init({no_app, AppId}) ->
  "{\"event\":\"pusher:error\",\"data\":{\"code\":4001,\"message\":\"Could not find app by key " ++ binary_to_list(AppId) ++ "\"}}".

ping() ->
  "{\"event\": \"pusher:pong\", \"data\": {}}".

success(ChannelName) ->
  "{\"event\": \"pusher_internal:connection_succeedeed\", \"data\": {}, \"channel\": \"" ++ binary_to_list(ChannelName) ++ "\"}".