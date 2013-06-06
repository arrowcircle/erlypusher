-module(websocket_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

check_key(Req) ->
  {AppKey, _Req2} = cowboy_req:binding(key, Req),
  case erlypusher_config:app_by_key(AppKey) of
    error ->
      {error, AppKey};
    {_Key, {AppId, _, _}} ->
      AppId
  end.

websocket_init(_Any, Req, _Opt) ->
  SocketId = uuid:to_string(uuid:v4()),
  Pid = request_parser:get_pid_from_req(Req),
  gproc:reg({p, g, socket_id}, SocketId),

  case check_key(Req) of
    {error, Key} ->
      Pid ! json_responder:response({error_no_app, Key});
    _AppId ->
      Pid ! json_responder:response({ok_connection, SocketId})
  end,

  {ok, Req, undefined_state}.

websocket_handle({text, Data}, Req, State) ->
  {Dict, Req2} = ws_parser:parse(Req, Data),
  Val = client_validator:check(Dict),
  case Val of
    {error, no_app_by_key, Key} ->
      Resp = channel:init({no_app, Key});
    {error, authentication_error} ->
      % auth error here;
      Resp = "";
    ok ->
      Resp = channel:handle(Dict)
  end,

  % Resp = respond_to_request(Data, Req2),
  {reply, {text, Resp}, Req2, State, hibernate};

websocket_handle(_Any, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
  {reply, {text, _Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.