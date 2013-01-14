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

% init(_Any, _Req, _Opt) ->
%     {upgrade, protocol, cowboy_http_websocket}.
make_ok_connection_response(Socket_id) ->
    Temp = "{\"event\": \"pusher:connection_established\", \"data\": {\"socket_id\": \"" ++ Socket_id,
    Temp ++ "\"}}".

make_ok_subscribe_channel_response(Channel_name) ->
  A = "{\"event\": \"pusher_internal:connection_succeedeed\", \"data\": {}, \"channel\": \"",
  B = A ++ binary_to_list(Channel_name),
  B ++ "\"}".

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

respond_to_action(<<"pusher:subscribe">>, Socket_id, Channel_name) ->
  gproc:send({p, l, Socket_id}, make_ok_subscribe_channel_response(Channel_name)).

websocket_init(_Any, Req, _Opt) ->
    Socket_id = uuid:to_string(uuid:v4()),
    gproc:reg({p, l, Socket_id}),
    gproc:send({p, l, Socket_id}, make_ok_connection_response(Socket_id)),
  {ok, Req, undefined, hibernate}.

% subscribe to channel
websocket_handle({text, Data}, Req, State) ->
  Channel_name = get_channel_name(Data),
  Resp = make_ok_subscribe_channel_response(Channel_name),
  {reply, {text, Resp}, Req, State, hibernate};

% ok connection
websocket_handle({text, Data}, Req, State) ->
  Channel_name = get_channel_name(Data),
  Resp = make_ok_subscribe_channel_response(Channel_name),
  {reply, {text, Resp}, Req, State, hibernate};


websocket_handle(_Any, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
  {reply, {text, _Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.