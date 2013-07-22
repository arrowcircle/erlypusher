-module(erlypusher_presence).

-compile(export_all).

-define(PRESENCE, erlypusher_presence).

-record(presence, {app_id, channel_id, uuid, info, created_at}).

start() ->
  server_util:start(?PRESENCE, {erlypusher_presence, run, [true]}).

stop() ->
  server_util:stop(?PRESENCE).

init_presence() ->
  mnesia:create_schema(node()),
  mnesia:start(),
  try
    mnesia:table_info(type, presence)
  catch
    exit: _ ->
      mnesia:create_table(presence, [
          {attributes, record_info(fields, presence)},
          {type, bag},
          {ram_copies, node()}
      ])
  end.

run(FirstTime) ->
  if 
    FirstTime == true ->
      init_presence(),
      run(false);
    true ->
      receive
        {subscribe, UserInfo, AppId} ->
          io:format("\n~p: Received subscribtion event: ~p\n", [AppId, UserInfo]),
          run(FirstTime);
        {list, ChannelId, AppId} ->
          io:format("\n~p: Received list event: ~p\n", [AppId, ChannelId]),
          run(FirstTime);
        {unsubscribe, UserId, AppId} ->
          io:format("\n~p: Received unsubscribe event: ~p\n", [AppId, UserId]),
          run(FirstTime);
        _ ->
          io:format("\n: Received unknown event\n"),
          run(FirstTime)
      end
    end.

