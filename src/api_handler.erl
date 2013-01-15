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
  gproc:send({p, l, ChannelName}, Message),
  {ok, Req4} = cowboy_http_req:reply(200, [], [<<"ok">>], Req3),
  {ok, Req4, State}.

maybe_event(_, true, Req) ->
  {Name, Req2} = cowboy_req:val(<<"name">>, Req),
  {Data, Req3} = cowboy_req:val(<<"data">>, Req2),
  {SocketId, Req4} = cowboy_req:val(<<"socket_id">>, Req3),
  {Path, Req5} = cowboy_http_req:path(Req4),
  [_|[AppId|_]] = Path,
  [_|[_|[_|[ChannelName|_]]]] = Path,
  event(Name, Data, SocketId, AppId, ChannelName, Req5);

maybe_event(<<"POST">>, false, Req) ->
  cowboy_req:reply(400, Req);

maybe_event(_, _, Req) ->
  cowboy_req:reply(405, Req).

make_event_response(Name, Data, SocketId, AppId, ChannelName) ->
  A = "{\"event\": \"" ++ binary_to_list(Name),
  B = A ++ "\", \"data\": {",
  C = B ++ "",%binary_to_list(Data),
  D = C ++ "}, \"channel\": \"",
  E = D ++ binary_to_list(ChannelName),
  F = E ++ "\", \"socket_id\": \"",
  G = F ++ binary_to_list(SocketId),
  H = G ++ "\"",
  G ++ "\"}".

event(Name, Data, SocketId, AppId, ChannelName, Req) ->
  Message = make_event_response(Name, Data, SocketId, AppId, ChannelName),
  gproc:send({p, l, ChannelName}, Message),
  cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], <<"ok">>, Req);

event(_, _, _, _, _, Req) ->
  cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req).

terminate(_Req, _State) ->
  ok.
