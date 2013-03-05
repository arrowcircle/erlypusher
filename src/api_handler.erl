-module(api_handler).
-export([init/3, handle/2, terminate/3]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.

% handle helpers
send_to_channel(Name, Data, ChannelName, Id) ->
  Message = json_responder:response(event, {Name, Data, ChannelName}),
  gproc:send({p, g, {Id, ChannelName}}, Message).

send_to_channels(_Name, _Data, _Id, []) ->
  ok;

send_to_channels(Name, Data, Id, Channels) ->
  [Channel|NewChannels] = Channels,
  send_to_channel(Name, Data, Channel, Id),
  send_to_channels(Name, Data, Id, NewChannels).

response_channels(Req, State, Json, Id, Channels) ->
  {[{<<"name">>, Name}, _, _]} = Json,
  {[_, _, {<<"data">>, Data}]} = Json,
  send_to_channels(Name, Data, Id, Channels),
  ok(Req, State).

get_channles_list(Json) ->
  {[{<<"name">>, _}, {<<"channels">>, Channels},_]} = Json,
  Channels.

% {[{<<"name">>,<<"an_event">>},
%         {<<"channels">>,[<<"chanelle">>,<<"truba">>,<<"duba">>]},
%         {<<"data">>,<<"{\"some\":\"data\"}">>}]}

check_channel_array(Req, State, Json, Id) ->
  ChannelsList = get_channles_list(Json),
  case ChannelsList of
    [] ->
      wrong_channel(Req, State);
    Channels ->
      response_channels(Req, State, Json, Id, Channels)
  end.

check_channel(Req, State, Json, Id) ->
  {ChannelIdUrl, Req2} = cowboy_req:binding(channel_id, Req),
  case ChannelIdUrl of
    undefined ->
      check_channel_array(Req2, State, Json, Id);
    ChannelId ->
      response_channels(Req2, State, Json, Id, [ChannelId])
  end.

check_body(Req, State, Id) ->
  Body = cowboy_req:body(Req),
  case Body of
    {ok, Json, Req2} ->
      BodyJson = jiffy:decode(Json),
      check_channel(Req2, State, BodyJson, Id);
    {error, _Reason} ->
      no_body(Req, State)
  end.

check_signature(Body, AuthKey, AuthSignature, AuthTimestamp, AuthVersion) ->
  % или error_timestamp или error
  ok.

parse_params(Req) ->
  {AuthKey, Req2} = cowboy_req:qs_val(<<"auth_key">>, Req),
  {AuthSignature, Req3} = cowboy_req:qs_val(<<"auth_signature">>, Req2),
  {Body, Req4} = cowboy_req:qs_val(<<"body_md5">>, Req3),
  {AuthTimestamp, Req5} = cowboy_req:qs_val(<<"auth_timestamp">>, Req4),
  {AuthVersion, Req6} = cowboy_req:qs_val(<<"auth_varsion">>, Req5),
  {{Body, AuthKey, AuthSignature, AuthTimestamp, AuthVersion}, Req6}.

check_auth(Req, State, {Id, {Key, Secret, Name}}) ->
  {{Body, AuthKey, AuthSignature, AuthTimestamp, AuthVersion}, Req2} = parse_params(Req),
  case AuthKey of
    Key ->
      case check_signature(Body, AuthKey, AuthSignature, AuthTimestamp, AuthVersion) of
        ok ->
        % actual code here
          check_body(Req2, State, Id);
        error ->
          wrong_secret(Req2, State);
        error_timestamp ->
          wrong_timestamp(Req2, State, AuthTimestamp)
      end;
    _ ->
      wrong_key(Req2, State)
  end.

% handle block

handle(Req, State) ->
  {AppId, Req2} = cowboy_req:binding(app_id, Req),
  case erlypusher_config:app_by_id(AppId) of
    error ->
      bad_app(Req2, State);
    {ok, AppHash} ->
      check_auth(Req2, State, {AppId, AppHash})
  end.

% Common responses

wrong_channel(Req, State) ->
  {ok, Req2} = cowboy_req:reply(500, [{<<"content-type">>, <<"application/json">>}], [<<"No channel">>], Req),
  {ok, Req2, State}.

wrong_timestamp(Req, State, Timestamp) ->
  {ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"application/json">>}], [<<"Timestamp expired: Given timestamp (">>+ Timestamp + <<") not within 600s of server time (">> + erlang:timestamp() + <<")">>], Req),
  {ok, Req2, State}.

no_body(Req, State) ->
  {ok, Req2} = cowboy_req:reply(500, [{<<"content-type">>, <<"application/json">>}], [<<"NO Body found">>], Req),
  {ok, Req2, State}.

ok(Req, State) ->
  {ok, Req2} = cowboy_req:reply(202, [{<<"content-type">>, <<"application/json">>}], [<<"202 ACCEPTED">>], Req),
  {ok, Req2, State}.

bad_app(Req, State) ->
  {ok, Req2} = cowboy_req:reply(400, [{<<"content-type">>, <<"application/json">>}], [<<"Not authorized for that app">>], Req),
  {ok, Req2, State}.

wrong_key(Req, State) ->
  {ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"application/json">>}], [<<"Unknown auth_key">>], Req),
  {ok, Req2, State}.

wrong_secret(Req, State) ->
  {ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"application/json">>}], [<<"Invalid signature: you should have sent HmacSHA256Hex(\"POST/apps/38128/events\nauth_key=9420a8ef0031a6153350&auth_timestamp=1362068936&auth_version=1.0&body_md5=f526bda6bdd083b269a52680132c1e4c\", your_secret_key), but you sent \"5688f4c254b912be46ad354c77b014bfa0cbd27c0e90a74e07c6143dc3dc7a2b\"">>], Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
