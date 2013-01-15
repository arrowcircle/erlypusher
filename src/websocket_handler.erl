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

make_ok_connection_response(Socket_id) ->
    Temp = "{\"event\": \"pusher:connection_established\", \"data\": {\"socket_id\": \"" ++ Socket_id ++ "\"}}",
    Temp.

make_ok_subscribe_channel_response(Channel_name) ->
  A = "{\"event\": \"pusher_internal:connection_succeedeed\", \"data\": {}, \"channel\": \"",
  B = A ++ binary_to_list(Channel_name) ++ "\"}",
  B.

make_ok_ping_response() ->
  "{\"event\": \"pusher:pong\", \"data\": {}}".

get_channel_name(Data) ->
  {struct, Json} = mochijson2:decode(binary_to_list(Data)),
  [_|Json_tail] = Json,
  [Json_tail_first|_] = Json_tail,
  {<<"data">>, {struct, [Data_head|_]}} = Json_tail_first,
  {<<"channel">>, Channel_name} = Data_head,
  Channel_name.

get_action_name(Data) ->
  {struct, Json} = mochijson2:decode(binary_to_list(Data)),
  [Head|_] = Json,
  {_, Action_name} = Head,
  Action_name.

get_pid_from_req(Req) ->
  element(5, Req).

respond_to_action(<<"pusher:subscribe">>, Data, _) ->
  ChannelName = get_channel_name(Data),
  gproc:reg({p, l, ChannelName}),
  make_ok_subscribe_channel_response(ChannelName);

respond_to_action(<<"pusher:unsubscribe">>, Data, Req) ->
  ChannelName = get_channel_name(Data),
  Pid = get_pid_from_req(Req),
  gproc:unreg({p, l, ChannelName}, Pid),
  make_ok_subscribe_channel_response(ChannelName);

respond_to_action(<<"pusher:ping">>, Data, _) ->
  make_ok_ping_response().

websocket_init(_Any, Req, _Opt) ->
  Socket_id = uuid:to_string(uuid:v4()),
  Pid = get_pid_from_req(Req),
  Pid ! make_ok_connection_response(Socket_id),
  {ok, Req, undefined, hibernate}.

websocket_handle({text, Data}, Req, State) ->
  Resp = respond_to_action(get_action_name(Data), Data, Req),
  {reply, {text, Resp}, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
  {reply, {text, _Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.