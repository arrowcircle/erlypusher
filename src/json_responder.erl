-module(json_responder).

-export([response/0, response/1]).

response({ok_connection, SocketId}) ->
  "{\"event\": \"pusher:connection_established\", \"data\": {\"socket_id\": \"" ++ SocketId ++ "\"}}";

response({ok_common_channel, ChannelName}) ->
  A = "{\"event\": \"pusher_internal:connection_succeedeed\", \"data\": {}, \"channel\": \"",
  B = A ++ binary_to_list(ChannelName) ++ "\"}",
  B;

response({ping}) ->
  "{\"event\": \"pusher:pong\", \"data\": {}}";

% errors

response({error_no_app, AppId}) ->
  "{\"type\":\"PusherError\",\"data\":{\"code\":4001,\"message\":\"Could not find app by key " ++ binary_to_list(AppId) ++ "\"}}";

response(_Any) ->
  ok.

response() ->
  ok.