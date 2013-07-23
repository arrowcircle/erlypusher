-module(erlypusher_presence_store_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
  erlypusher_presence_store:start_link().

clean() ->
  erlypusher_presence_store:stop().

store_and_read() ->
  erlypusher_presence_store:subscribe("AppId", "ChannelId", "UserInfo", "Pid", "Uuid"),
  erlypusher_presence_store:user_info("AppId", "ChannelId", "Uuid").

store_and_delete() ->
  erlypusher_presence_store:subscribe("AppId", "ChannelId", "UserInfo", "Pid", "Uuid"),
  erlypusher_presence_store:unsubscribe("AppId", "ChannelId", "Uuid"),
  erlypusher_presence_store:user_info("AppId", "ChannelId", "Uuid").

channel_store_and_read() ->
  erlypusher_presence_store:subscribe("AppId", "ChannelId", "UserInfo", "Pid", "Uuid"),
  erlypusher_presence_store:subscribe("AppId", "ChannelId", "UserInfo1", "Pid1", "Uuid1"),
  erlypusher_presence_store:channel_info("AppId", "ChannelId").

erlypusher_presence_store_tests(_I) ->
  {_, Now, _} = erlang:now(),
  [?_assertEqual([{presence, "AppId", "ChannelId", "Uuid", "UserInfo", "Pid", Now}], store_and_read()),
   ?_assertEqual([], store_and_delete()),
   ?_assertEqual([{presence, "AppId", "ChannelId", "Uuid", "UserInfo", "Pid", Now}, {presence, "AppId", "ChannelId", "Uuid1", "UserInfo1", "Pid1", Now}], channel_store_and_read())].

generate_erlypusher_presence_store_test_() ->
  {setup, fun() -> setup() end, fun(_) -> clean() end, fun erlypusher_presence_store_tests/1}.