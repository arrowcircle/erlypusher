-module(websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init(_Any, _Req, _Opt) ->
   {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_Any, Req, _Opt) ->
       gproc:reg({p,l, main_room}),
  {ok, Req, undefined, hibernate}.

websocket_handle({text,Data}, Req, State) ->
     gproc:send({p,l,main_room},Data),
        {ok, Req,State, hibernate};
websocket_handle(_Any, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
   io:format("~p~n",[_Info]),
  {reply, {text,_Info}, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.