-module(client_validator_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NO_APP, dict:from_list([{"app", {no_key, "123"}}])).
-define(APP, dict:from_list([{"app", {"id", "key", "secret", "name"}}])).
-define(PING, dict:from_list([{"event", <<"pusher:ping">>}])).
-define(UNSUBSCRIBE, dict:from_list([{"event", <<"pusher:unsubscribe">>}])).

-define(COMMON_CHANNEL, dict:from_list([{"event", <<"pusher:subscribe">>}, {"channel", <<"common_channel">>}, {"channel_type", common}])).
-define(PRIVATE_CHANNEL, dict:from_list([{"event", <<"pusher:subscribe">>}, {"app", {"id", "key", "secret", "name"}}, {"channel", <<"private-channel">>}, {"channel_type", private}, {"auth", <<"AUTH_STRING">>}])).
-define(WRONG_PRIVATE_CHANNEL, dict:from_list([{"event", <<"pusher:subscribe">>}, {"app", {"id", "key", "secret", "name"}}, {"channel", <<"private-channel">>}, {"channel_type", private}])).

client_validator_test_() ->
% 1. check app by key
%      - {error_no_key, AppKey} if no app found
% 2. Get action name
%         - ping => pong
%         - unsibcribe => unsubscribe
%         - subscribe => subscribe
  [?_assertEqual({error, no_app_by_key}, client_validator:app(?NO_APP)),
   ?_assertEqual(ok, client_validator:app(?APP)),
   ?_assertEqual(ok, client_validator:event(?PING)),
   ?_assertEqual(ok, client_validator:event(?UNSUBSCRIBE)),
   ?_assertEqual(ok, client_validator:event(?COMMON_CHANNEL)),
   ?_assertEqual(ok, client_validator:channel(?COMMON_CHANNEL)),
   ?_assertEqual(ok, client_validator:private(?PRIVATE_CHANNEL)),
   ?_assertEqual(ok, client_validator:private(?WRONG_PRIVATE_CHANNEL))].