-module(api_handler).
-export([init/3, handle/2, terminate/3]).

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {AppId, Req2} = cowboy_req:binding(app_id, Req),
  case erlypusher_config:app_by_id(AppId) of
    error ->
      ok;
    {ok, _} ->
      ok
  end,
  {EventName, Req3} = cowboy_req:qs_val(<<"name">>, Req2, <<"">>),
  {EventData, Req4} = cowboy_req:qs_val(<<"data">>, Req3, <<"">>),
  {EventSocket, Req5} = cowboy_req:qs_val(<<"socket_id">>, Req4, <<"">>),
  {ChannelName, Req6} = cowboy_req:binding(channel_id, Req5),
  case EventName of
    <<"">> ->
      {ok, Req7} = cowboy_req:reply(400, [], [], Req);
    EventName ->
      Message = make_event_response(EventName, EventData, EventSocket, ChannelName),
      gproc:send({p, g, {AppId, ChannelName}}, Message),
      {ok, Req7} = cowboy_req:reply(200, [], [<<"ok">>], Req6)
  end,
  {ok, Req7, State}.

make_event_response(Name, Data, SocketId, ChannelName) ->
  jiffy:encode({[{<<"event">>, Name},
                 {<<"data">>, Data},
                 {<<"channel">>, ChannelName},
                 {<<"socket_id">>, SocketId}]}),

terminate(_Reason, _Req, _State) ->
  ok.
