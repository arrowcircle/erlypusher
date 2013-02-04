-module(authenticator).

-export([sign/2]).

sign(SignString, Secret) ->
  string:to_lower(hmac:hexlify(hmac:hmac256(Secret, SignString))).

can_join({private, ChannelName, SocketId, Auth, CustomString}) ->
  SignStr = SocketId ++ ":" ++ binary_to_list(ChannelName) ++ ":" ++ CustomString
  Auth = sign(SignString, Secret).
