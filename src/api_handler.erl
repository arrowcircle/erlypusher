-module(api_handler).
-export([init/3, handle/2, terminate/3, handle/3]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.


parse_request(Req, State, Json) ->
  io:format("\n\n\n\n~p\n\n\n\n", [Json]).


handle(Req, State, AppHash) ->
  Body = cowboy_req:body(Req),
  case Body of
    {ok, Json, Req2} ->
      BodyJson = jiffy:decode(Json),
      parse_request(Req2, State, BodyJson);
    {error, Reason} ->
      io:format("no body found in API hander: ~p\n", [Reason]),
      Req2 = no_body(Req),
      {ok, Req2, State}
  end.

handle(Req, State) ->
  {AppId, Req2} = cowboy_req:binding(app_id, Req),
  case erlypusher_config:app_by_id(AppId) of
    error ->
      bad_app(Req2, State);
    {ok, AppHash} ->
      handle(Req2, State, {AppId, AppHash})
  end.

no_body(Req) ->
  {ok, Req2} = cowboy_req:reply(500, [{<<"content-type">>, <<"application/json">>}], [<<"NO Body found">>], Req),
  Req2.

ok(Req) ->
  {ok, Req2} = cowboy_req:reply(202, [{<<"content-type">>, <<"application/json">>}], [<<"202 ACCEPTED">>], Req),
  Req2.

bad_app(Req, State) ->
  {ok, Req2} = cowboy_req:reply(400, [{<<"content-type">>, <<"application/json">>}], [<<"Not authorized for that app">>], Req),
  {ok, Req2, State}.

wrong_key(Req) ->
  {ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"application/json">>}], [<<"Unknown auth_key">>], Req).

wrong_secret(Req) ->
  {ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"application/json">>}], [<<"Invalid signature: you should have sent HmacSHA256Hex(\"POST/apps/38128/events\nauth_key=9420a8ef0031a6153350&auth_timestamp=1362068936&auth_version=1.0&body_md5=f526bda6bdd083b269a52680132c1e4c\", your_secret_key), but you sent \"5688f4c254b912be46ad354c77b014bfa0cbd27c0e90a74e07c6143dc3dc7a2b\"">>], Req).


  % {EventName, Req4} = cowboy_req:qs_val(<<"name">>, Req3, <<"">>),
  % {EventData, Req5} = cowboy_req:qs_val(<<"data">>, Req4, <<"">>),
  % {EventSocket, Req6} = cowboy_req:qs_val(<<"socket_id">>, Req5, <<"">>),
  % {ChannelName, Req7} = cowboy_req:qs_val(<<"channels">>, Req6),
  % case EventName of
  %   <<"">> ->
  %     {ok, Req8} = cowboy_req:reply(400, [], [], Req);
  %   EventName ->
  %     Message = make_event_response(EventName, EventData, EventSocket, ChannelName),
  %     gproc:send({p, g, {AppId, ChannelName}}, Message),
  %     {ok, Req8} = cowboy_req:reply(202, [{<<"content-type">>, <<"application/json">>}], [<<"202 ACCEPTED">>], Req7)
  % end,

make_event_response(Name, Data, SocketId, ChannelName) ->
  jiffy:encode({[{<<"event">>, Name},
                 {<<"data">>, Data},
                 {<<"channel">>, ChannelName},
                 {<<"socket_id">>, SocketId}]}).

terminate(_Reason, _Req, _State) ->
  ok.
