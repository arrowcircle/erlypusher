-module(websocket_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

respond_common_channel(ChannelName, _Req, AppId) ->
  % ChannelName = request_parser:get_channel_name(Data),
  gproc:reg({p, g, {AppId, ChannelName}}),
  json_responder:response({ok_channel, ChannelName}).

respond_private_channel(ChannelName, Auth, Req, AppId) ->
  % ChannelName = request_parser:get_channel_name(Data),
  {ok, {_Key, Secret, _Name}} = erlypusher_config:app_by_id(AppId),
  SocketId = gproc:get_value({p, g, socket_id}),
  {ok, {Key, Secret, _Name}} = erlypusher_config:app_by_id(AppId),
  case authenticator:can_join(ChannelName, SocketId, Auth, "", Secret, Key) of
    ok ->
      gproc:reg({p, g, {AppId, ChannelName}}),
      json_responder:response({ok_channel, ChannelName});
    _ ->
      error
      % write response for wrong auth
  end.

check_channel_type(ChannelName) ->
  String = binary_to_list(ChannelName),
  case string:str(String, "presence-") of
    0 ->
      case string:str(String, "private-") of
        0 ->
          common;
        _ ->
          private
      end;
    _ ->
      presence
  end.

respond_to_action({<<"pusher:subscribe">>, AppId}, Data, Req) ->
% {"event":"pusher:subscribe","data":{"channel":"private-MY_CHANNEL","auth":"9ba776377551a1d716b8:329329be89573affd6895f3026a19913f0d2712e95d313830fbb45160dfc3e90"}}
  ChannelName = request_parser:get_channel_name(Data),
  case check_channel_type(ChannelName) of
    private ->
      % io:format("Received common connection\n"),
      Auth = request_parser:get_auth(Data),
      respond_private_channel(ChannelName, Auth, Req, AppId);
    presence ->
      % io:format("Received presence connection\n"),
      % presence channel
      ok;
    common ->
      % io:format("Received common connection\n"),
      respond_common_channel(ChannelName, Req, AppId)
  end;

respond_to_action({<<"pusher:unsubscribe">>, _AppId}, Data, Req) ->
  ChannelName = request_parser:get_channel_name(Data),
  Pid = request_parser:get_pid_from_req(Req),
  gproc:unreg({p, g, ChannelName}, Pid),
  json_responder:respose({ok_channel, ChannelName});

respond_to_action({<<"pusher:ping">>, _AppId}, _Data, _Req) ->
  json_responder:response({ping}).

check_key(Req) ->
  {AppKey, _Req2} = cowboy_req:binding(key, Req),
  case erlypusher_config:app_by_key(AppKey) of
    error ->
      {error, AppKey};
    {_Key, {AppId, _, _}} ->
      AppId
  end.

respond_to_request(Data, Req) ->
  case check_key(Req) of
    {error, Key} ->
      json_responder:response({error_no_app, Key});
    AppId ->
      ActionName = request_parser:get_action_name(Data),
      respond_to_action({ActionName, AppId}, Data, Req)
  end.

websocket_init(_Any, Req, _Opt) ->
  % check if app_id and key exist
  SocketId = uuid:to_string(uuid:v4()),
  Pid = request_parser:get_pid_from_req(Req),
  gproc:reg({p, g, socket_id}, SocketId),
  % get value by this
  % gproc:get_value({p, g, socket_id}, erlang:list_to_pid("<0.286.0>")).

  case check_key(Req) of
    {error, Key} ->
      Pid ! json_responder:response({error_no_app, Key});
    _AppId ->
      Pid ! json_responder:response({ok_connection, SocketId})
  end,

  {ok, Req, undefined_state}.

websocket_handle({text, Data}, Req, State) ->
  Resp = respond_to_request(Data, Req),
  {reply, {text, Resp}, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
  {reply, {text, _Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.