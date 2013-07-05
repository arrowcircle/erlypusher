-module(erlypusher_authenticator).

-export([can_join/6, sign/2, id/1, md5_check/2, signature_check/5, format_params/1]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

sign(SignString, Secret) ->
  list_to_binary(string:to_lower(hmac:hexlify(hmac:hmac256(Secret, SignString)))).

can_join(ChannelName, SocketId, Auth, _CustomString, Secret, Key) ->
  AuthString = binary_to_list(Auth),
  SignString = SocketId ++ ":" ++ binary_to_list(ChannelName),
  CheckAuth = binary_to_list(Key) ++ ":" ++ binary_to_list(sign(SignString, Secret)),
  case CheckAuth of
    AuthString ->
      ok;
    _ ->
      error
  end.

id(Req) ->
  {AppId, Req2} = cowboy_req:binding(app_id, Req),
  {ok, Dict} = application:get_env(erlypusher, app_ids),
  App = dict:find(AppId, Dict),
  case App of
    {ok, AppInfo} ->
      {ok, AppInfo, Req2};
    _ ->
      {error, Req2}
  end.

format_list(Key, Array) ->
  format_list(Key, Array, <<"">>).

format_list(_Key, [], Res) ->
  Res;

format_list(Key, Array, <<"">>) ->
  [H|T] = Array,
  Res1 = <<Key/binary, <<"[]=">>/binary, H/binary>>,
  format_list(Key, T, Res1);

format_list(Key, Array, Res) ->
  [H|T] = Array,
  format_list(Key, T, <<Res/binary, <<"&">>/binary, Key/binary, <<"[]=">>/binary, H/binary>>).

format_params_list({Key, Value}) ->
  case erlypusher_helper:type_of(Value) of
    list ->
      format_list(Key, Value);
    bitstring ->
      <<Key/binary, <<"=">>/binary, Value/binary>>
  end.

format_params(Params) ->
  join_params(lists:map(fun format_params_list/1, Params)).

join_params(Array) ->
  join_params(<<"">>, Array).

join_params(Res, []) ->
  Res;

join_params(<<"">>, Array) ->
  [H|T] = Array,
  join_params(<<H/binary>>, T);

join_params(Res, Array) ->
  [H|T] = Array,
  Res1 = <<Res/binary, <<"&">>/binary, H/binary>>,
  join_params(Res1, T).

join_signature_string(Method, Url, ParamsString) ->
  <<Method/binary, <<"\n">>/binary, Url/binary, <<"\n">>/binary, ParamsString/binary>>.

signature(ParamsHash, Method, Url, Secret) ->
  Params = dict:from_list(ParamsHash),
  ParamsNew = dict:to_list(dict:erase(<<"auth_signature">>, Params)),
  ParamsForAuth = lists:sort(fun({A, _}, {B, _}) -> A < B end, ParamsNew),
  SignString = erlypusher_authenticator:format_params(ParamsForAuth),
  sign(join_signature_string(Method, Url, SignString), Secret).

timestamp_check(Params) ->
  {ok, AuthTimeStampString} = dict:find(<<"auth_timestamp">>, dict:from_list(Params)),
  {OsMega, OsSeconds, _} = os:timestamp(),
  AuthTimeStamp = binary_to_list(AuthTimeStampString),
  Mega = list_to_integer(string:substr(AuthTimeStamp, 1, 4)),
  Seconds = list_to_integer(string:substr(AuthTimeStamp, 5)),
  if
    Mega == OsMega ->
      if
        abs(OsSeconds - Seconds) < 600 ->
          ok;
        true ->
          error
      end;
    true ->
      error
  end.

signature_check(Signature, Params, Method, Url, Secret) ->
  case signature(Params, Method, Url, Secret) of
    Signature ->
      case timestamp_check(Params) of
        ok ->
          ok;
        _ ->
          error_timestamp
      end;
    _ ->
      io:format("Wrong Signature: Request Signature: ~p\n Server signature: ~p\n\n", [Signature, signature(Params, Method, Url, Secret)]),
      error
  end.

md5_check(Body, BodyMD5) ->
  case md5_hexdigest(Body) of
    BodyMD5 ->
      ok;
    _MD5 ->
      io:format("\n\n Wrong MD5: ~p\nBody was:\n~p\n\n", [BodyMD5, Body]),
      error
  end.

md5_hexdigest(String) ->
  list_to_binary(string:to_lower(
    lists:flatten(
      lists:map(
        fun(V) ->
          case httpd_util:integer_to_hexlist(V) of
            [A, B] -> [A, B];
            [B] -> [$0, B]
          end
        end,
    binary_to_list(erlang:md5(String))
  )))).

