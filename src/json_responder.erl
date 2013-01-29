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

response(_Any) ->
  ok.

response() ->
  ok.