-module(channel).
-export([handle/1, init_connection/1]).

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

check_key(Req) ->
  {AppKey, _Req2} = cowboy_req:binding(key, Req),
  case erlypusher_config:app_by_key(AppKey) of
    error ->
      {error, AppKey};
    {_Key, {AppId, _, _}} ->
      AppId
  end.

init_connection(Req) ->
  SocketId = uuid:to_string(uuid:v4()),
  Pid = request_parser:get_pid_from_req(Req),
  case check_key(Req) of
    {error, Key} ->
      Pid ! json_responder:response({error_no_app, Key});
    _AppId ->
      gproc:reg({p, g, socket_id}, SocketId),
      Pid ! json_responder:response({ok_connection, SocketId})
  end,
  Req.

handle(Dict) ->
  {ok, [Event]} = dict:find("event", Dict),
  handle(Event, Dict).

handle(<<"pusher:ping">>, _Dict) ->
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