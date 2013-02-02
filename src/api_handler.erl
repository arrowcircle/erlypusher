-module(api_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {AppId, Req2} = cowboy_http_req:binding(app_id, Req),
  {EventName, _} = cowboy_http_req:qs_val(<<"name">>, Req, <<"">>),
  {EventData, _} = cowboy_http_req:qs_val(<<"data">>, Req, <<"">>),
  {EventSocket, _} = cowboy_http_req:qs_val(<<"socket_id">>, Req, <<"">>),
  if EventName =:= <<"">> ->
    {ok, Req5} = cowboy_http_req:reply(400, [], [], Req),
    {ok, Req5, State};
    true -> always_true
  end,
  {ChannelName, Req3} = cowboy_http_req:binding(channel_id, Req2),
  Message = make_event_response(EventName, EventData, EventSocket, AppId, ChannelName),
  gproc:send({p, g, ChannelName}, Message),
  {ok, Req4} = cowboy_http_req:reply(200, [], [<<"ok">>], Req3),
  {ok, Req4, State}.

make_event_response(Name, Data, SocketId, AppId, ChannelName) ->
  A = "{\"event\": \"" ++ binary_to_list(Name),
  B = A ++ "\", \"data\": " ++ "\"" ++ binary_to_list(Data),
  C = B ++ "\", \"channel\": \"" ++ binary_to_list(ChannelName),
  D = C ++ "\", \"socket_id\": \"" ++ binary_to_list(SocketId) ++ "\"}",
  D.

terminate(_Req, _State) ->
  ok.
