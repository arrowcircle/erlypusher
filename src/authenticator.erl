-module(authenticator).

-export([sign/2]).

sign(SignString, Secret) ->
  string:to_lower(hmac:hexlify(hmac:hmac256(Secret, SignString))).
