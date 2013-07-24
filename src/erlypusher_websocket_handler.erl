-module(erlypusher_websocket_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, _Opt) ->
  Req2 = erlypusher_channel:init_connection(Req),
  {ok, Req2, undefined_state}.

websocket_handle({text, Data}, Req, State) ->
  {Dict, Req2} = erlypusher_ws_parser:parse(Req, Data),
  Val = erlypusher_client_validator:check(Dict),

  case Val of
    {error, no_app_by_key, Key} ->
      Resp = erlypusher_channel:init({no_app, Key});
    {error, authentication_error} ->
      % auth error here;
      Resp = "";
    ok ->
      Resp = erlypusher_channel:handle(Dict)
  end,

  {reply, {text, Resp}, Req2, State, hibernate};

websocket_handle(_Any, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
  {reply, {text, _Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.