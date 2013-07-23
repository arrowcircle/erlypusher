-module(erlypusher_client_validator_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NO_APP, dict:from_list([{"app", {no_key, "123"}}])).
-define(APP, dict:from_list([{"app", {"id", "key", "secret", "name"}}])).
-define(PING, dict:from_list([{"event", <<"pusher:ping">>}])).
-define(UNSUBSCRIBE, dict:from_list([{"event", <<"pusher:unsubscribe">>}])).

-define(COMMON_CHANNEL, dict:from_list([{"event", <<"pusher:subscribe">>}, {"app", {"id", "key", "secret", "name"}}, {"channel", <<"channel">>}, {"channel_type", common}])).

-define(PRIVATE_CHANNEL, dict:from_list([{"event", <<"pusher:subscribe">>}, {"app", {"id", "key", "secret", "name"}}, {"channel", <<"private-channel">>}, {"channel_type", private}, {"auth", <<"AUTH_STRING">>}])).
-define(WRONG_PRIVATE_CHANNEL, dict:from_list([{"event", <<"pusher:subscribe">>}, {"app", {"id", "key", "secret", "name"}}, {"channel", <<"private-channel">>}, {"channel_type", private}])).

erlypusher_client_validator_test_() ->
  [?_assertEqual({error, no_app_by_key, "123"}, erlypusher_client_validator:check(?NO_APP)),
   ?_assertEqual(ok, erlypusher_client_validator:check(?COMMON_CHANNEL)),
   ?_assertEqual(ok, erlypusher_client_validator:event(?PING)),
   ?_assertEqual(ok, erlypusher_client_validator:event(?UNSUBSCRIBE)),
   ?_assertEqual(ok, erlypusher_client_validator:event(?COMMON_CHANNEL)),
   ?_assertEqual(ok, erlypusher_client_validator:channel(?COMMON_CHANNEL))
   ].