-module(erlypusher_request_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_DATA, <<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"MY_CHANNEL\"}}">>).

generate_request_parser_test_() ->
  [?_assertEqual(<<"pusher:subscribe">>, erlypusher_request_parser:get_action_name(?TEST_DATA)),
   ?_assertEqual(<<"MY_CHANNEL">>, erlypusher_request_parser:get_channel_name(?TEST_DATA))].