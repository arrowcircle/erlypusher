-module(authenticator).

-export([sign/2, id/1]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

sign(SignString, Secret) ->
  string:to_lower(hmac:hexlify(hmac:hmac256(Secret, SignString))).

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
