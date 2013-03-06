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

check_body(Req, State, Id, Body) ->
  case jiffy:decode(Body) of
    {error, _Reason} ->
      no_body(Req, State);
    Json ->
      check_channel(Req, State, Json, Id)
  end.

check_signature(Req, State, AuthSignature, Body, Key, Secret, Id) ->
  {ParamsHash, Req2} = cowboy_req:qs_vals(Req),
  {Method, Req3} = cowboy_req:method(Req2),
  {Url, Req4} = cowboy_req:path(Req3),
  case authenticator:signature_check(AuthSignature, ParamsHash, Method, Url, Secret) of
    ok ->
      check_body(Req4, State, Id, Body);
    error ->
      wrong_secret(Req4, State)
  end.

check_md5(BodyMD5, Body) ->
  case authenticator:md5_check(Body, BodyMD5) of
    ok ->
      ok;
    error ->
      error_md5
  end.
  % или error_timestamp или error

parse_params(Req) ->
  {BodyMD5, Req2} = cowboy_req:qs_val(<<"body_md5">>, Req),
  {AuthSignature, Req3} = cowboy_req:qs_val(<<"auth_signature">>, Req2),
  {AuthKey, Req4} = cowboy_req:qs_val(<<"auth_key">>, Req3),
  {ok, Body, Req5} = cowboy_req:body(Req4),
  {{BodyMD5, Body, AuthSignature, AuthKey}, Req5}.

check_auth(Req, State, {Id, {Key, Secret, _Name}}) ->
  {{BodyMD5, Body, AuthSignature, AuthKey}, Req2} = parse_params(Req),
  case AuthKey of
    Key ->
      case check_md5(BodyMD5, Body) of
        ok ->
          check_signature(Req2, State, AuthSignature, Body, Key, Secret, Id);
        error_md5 ->
          wrong_md5(Req2, State)
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

wrong_md5(Req, State) ->
  {ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"application/json">>}], [<<"Wrong MD5">>], Req),
  {ok, Req2, State}.

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
  {ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"application/json">>}], [<<"Invalid signature">>], Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
