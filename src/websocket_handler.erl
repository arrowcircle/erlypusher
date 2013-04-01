-module(websocket_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

respond_common_channel(Data, Req) ->
  case check_key(Req) of
    {error, Key} ->
      json_responder:response({error_no_app, Key});
    AppId ->
      ChannelName = request_parser:get_channel_name(Data),
      gproc:reg({p, g, {AppId, ChannelName}}),
      json_responder:response({ok_channel, ChannelName})
  end.

respond_private_channel(Data, Req) ->
  case check_key(Req) of
    error ->
      {AppKey, _Req2} = cowboy_req:binding(key, Req),
      json_responder:response({error_no_app, AppKey});
    AppId ->
      ChannelName = request_parser:get_channel_name(Data),
      gproc:reg({p, g, {AppId, ChannelName}}),
      json_responder:response({ok_channel, ChannelName})
  end.


check_key(Req) ->
  {AppKey, _Req2} = cowboy_req:binding(key, Req),
  case erlypusher_config:app_by_key(AppKey) of
    error ->
      {error, AppKey};
    {_Key, {AppId, _, _}} ->
      AppId
  end.

respond_to_action(<<"pusher:subscribe">>, Data, Req) ->
% {"event":"pusher:subscribe","data":{"channel":"private-MY_CHANNEL","auth":"9ba776377551a1d716b8:329329be89573affd6895f3026a19913f0d2712e95d313830fbb45160dfc3e90"}}
  respond_common_channel(Data, Req);

respond_to_action(<<"pusher:unsubscribe">>, Data, Req) ->
  ChannelName = request_parser:get_channel_name(Data),
  Pid = request_parser:get_pid_from_req(Req),
  gproc:unreg({p, g, ChannelName}, Pid),
  json_responder:respose({ok_channel, ChannelName});

respond_to_action(<<"pusher:ping">>, _Data, _Req) ->
  json_responder:response({ping}).

respond_to_request(Data, Req) ->
  ActionName = request_parser:get_action_name(Data),
  respond_to_action(ActionName, Data, Req).

websocket_init(_Any, Req, _Opt) ->
  % check if app_id and key exist
  SocketId = uuid:to_string(uuid:v4()),
  Pid = request_parser:get_pid_from_req(Req),
  {AppKey, Req2} = cowboy_req:binding(key, Req),
  case erlypusher_config:app_by_key(AppKey) of
    {ok, _} ->
      Pid ! json_responder:response({ok_connection, SocketId});
    error ->
      Pid ! json_responder:response({error_no_app, AppKey})
  end,
  {ok, Req2, undefined_state}.

websocket_handle({text, Data}, Req, State) ->
  Resp = respond_to_request(Data, Req),
  {reply, {text, Resp}, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
  {reply, {text, _Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.