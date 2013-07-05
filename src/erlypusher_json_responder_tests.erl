-module(erlypusher_json_responder_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OK_CONNECTION, "{\"event\": \"pusher:connection_established\", \"data\": {\"socket_id\": \"ABCDEF\"}}").
-define(OK_COMMON_CHANNEL, "{\"event\": \"pusher_internal:connection_succeedeed\", \"data\": {}, \"channel\": \"test channel\"}").
-define(OK_PING, "{\"event\": \"pusher:pong\", \"data\": {}}").
-define(ERROR_NO_APP, "{\"event\":\"pusher:error\",\"data\":{\"code\":4001,\"message\":\"Could not find app by key 765ec374ae0a69f4ce444\"}}").

generate_json_responder_test_() ->
  [?_assertEqual(?OK_CONNECTION, erlypusher_json_responder:response({ok_connection, "ABCDEF"})),
   ?_assertEqual(?OK_COMMON_CHANNEL, erlypusher_json_responder:response({ok_channel, <<"test channel">>})),
   ?_assertEqual(?OK_PING, erlypusher_json_responder:response({ping})),
   ?_assertEqual(ok, erlypusher_json_responder:response(any_shit)),
   ?_assertEqual(ok, erlypusher_json_responder:response()),
   ?_assertEqual(?ERROR_NO_APP, erlypusher_json_responder:response({error_no_app, <<"765ec374ae0a69f4ce444">>}))].