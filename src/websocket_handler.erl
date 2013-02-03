-module(websocket_handler).

-behaviour(cowboy_http_websocket_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
  case cowboy_http_req:header('Upgrade', Req) of
    {undefined, Req2} -> {ok, Req2, undefined};
    {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
    {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
  end.

respond_to_action(<<"pusher:subscribe">>, Data, _) ->
  ChannelName = request_parser:get_channel_name(Data),
  gproc:reg({p, g, ChannelName}),
  json_responder:response({ok_common_channel, ChannelName});

respond_to_action(<<"pusher:unsubscribe">>, Data, Req) ->
  ChannelName = request_parser:get_channel_name(Data),
  Pid = request_parser:get_pid_from_req(Req),
  gproc:unreg({p, g, ChannelName}, Pid),
  json_responder:respose({ok_common_channel, ChannelName});

respond_to_action(<<"pusher:ping">>, _Data, _Req) ->
  json_responder:respose({ping}).

respond_to_request(Data, Req) ->
  ActionName = request_parser:get_action_name(Data),
  respond_to_action(ActionName, Data, Req).

websocket_init(_Any, Req, _Opt) ->
  SocketId = uuid:to_string(uuid:v4()),
  Pid = request_parser:get_pid_from_req(Req),
  Pid ! json_responder:response({ok_connection, SocketId}),
  {ok, Req, undefined, hibernate}.

websocket_handle({text, Data}, Req, State) ->
  Resp = respond_to_request(Data, Req),
  {reply, {text, Resp}, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
  {reply, {text, _Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.