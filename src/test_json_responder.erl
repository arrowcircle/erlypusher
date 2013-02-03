-module(test_json_responder).

-include_lib("eunit/include/eunit.hrl").

-define(OK_CONNECTION, "{\"event\": \"pusher:connection_established\", \"data\": {\"socket_id\": \"ABCDEF\"}}").
-define(OK_COMMON_CHANNEL, "{\"event\": \"pusher_internal:connection_succeedeed\", \"data\": {}, \"channel\": \"test channel\"}").
-define(OK_PING, "{\"event\": \"pusher:pong\", \"data\": {}}").

generate_json_responder_test_() ->
  [?_assertEqual(?OK_CONNECTION, json_responder:response({ok_connection, "ABCDEF"})),
   ?_assertEqual(?OK_COMMON_CHANNEL, json_responder:response({ok_common_channel, <<"test channel">>})),
   ?_assertEqual(?OK_PING, json_responder:response({ping})),
   ?_assertEqual(ok, json_responder:response(any_shit)),
   ?_assertEqual(ok, json_responder:response())].