-module(authenticator).

-export([sign/2, id/1, md5_check/2, signature_check/5, format_params/1]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

sign(SignString, Secret) ->
  list_to_binary(string:to_lower(hmac:hexlify(hmac:hmac256(Secret, SignString)))).

can_join(private, ChannelName, SocketId, Auth, CustomString) ->
  SignString = SocketId ++ ":" ++ binary_to_list(ChannelName) ++ ":" ++ CustomString,
  Secret = "123",
  Auth = sign(SignString, Secret).

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
  format_list(Key, T, <<Res/binary, <<"&">>/binary, Key/binary, <<"[]=">>, H/binary>>).

format_params_list({Key, Value}) ->
  case helper:type_of(Value) of
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
  SignString = authenticator:format_params(ParamsForAuth),
  sign(join_signature_string(Method, Url, SignString), Secret).

signature_check(Signature, Params, Method, Url, Secret) ->
  case signature(Params, Method, Url, Secret) of
    Signature ->
      ok;
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

