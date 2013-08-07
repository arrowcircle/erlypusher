-module(erlypusher_channel).
-export([handle/1, init_connection/1]).

-include_lib("erlson/include/erlson.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

% api
api(event, {Name, Data, ChannelName}) ->
  jiffy:encode({[{<<"event">>, Name},
                 {<<"data">>, Data},
                 {<<"channel">>, ChannelName}]}).

% client

check_key(Req) ->
  {AppKey, _Req2} = cowboy_req:binding(key, Req),
  case erlypusher_config:app_by_key(AppKey) of
    error ->
      {error, AppKey};
    {_Key, {AppId, _, _}} ->
      AppId
  end.

init_connection(Req) ->
  SocketId = uuid:to_string(uuid:v4()),
  Pid = erlypusher_request_parser:get_pid_from_req(Req),
  case check_key(Req) of
    {error, Key} ->
      Pid ! erlypusher_json_responder:response({error_no_app, Key});
    _AppId ->
      gproc:reg({p, g, socket_id}, SocketId),
      Pid ! erlypusher_json_responder:response({ok_connection, SocketId})
  end,
  Req.

handle(Dict) ->
  {ok, [Event]} = dict:find("event", Dict),
  handle(Event, Dict).

handle(<<"pusher:ping">>, _Dict) ->
  ping();

handle(<<"pusher:unsubscribe">>, Dict) ->
  unsubscribe(Dict);

handle(<<"pusher:subscribe">>, Dict) ->
  {ok, [ChannelType]} = dict:find("channel_type", Dict),
  channel_subscription(ChannelType, Dict).

channel_subscription(common, Dict) ->
  simple_subscription(Dict);

channel_subscription(private, Dict) ->
  simple_subscription(Dict);

channel_subscription(presence, Dict) ->
  {ok, [Data]} = dict:find("data", Dict),
  presence_subscription(Dict).

simple_subscription(Dict) ->
  {ok, [ChannelName]} = dict:find("channel", Dict),
  {ok, [{AppId, _Key, _Secret, _Name}]} = dict:find("app", Dict),
  gproc:reg({p, g, {AppId, ChannelName}}),
  success(ChannelName).

presence_subscription(Dict) ->
  {ok, [ChannelName]} = dict:find("channel", Dict),
  {ok, [Data]} = dict:find("data", Dict),
  ErlsonData = erlson:from_json(Data),
  case erlson:get_value(user_id, ErlsonData, undefined) of
    undefined ->
      no_user_id(ChannelName);
    UserId ->
      {ok, [{AppId, _Key, _Secret, _Name}]} = dict:find("app", Dict),
      erlypusher_presence_store:subscribe(AppId, ChannelName, ErlsonData.user_info, self(), UserId),
      {Ids, Hash} = prepare_channel_info(erlypusher_presence_store:channel_info(AppId, ChannelName)),
      InfoHash = {[{count, lists:flatlength(Ids)}, {ids, Ids}, {hash, Hash}]},
      PresenceHash = {[{presence, InfoHash}]},
      gproc:send({p, g, {AppId, ChannelName}}, member_added(ChannelName, Data)),
      gproc:reg({p, g, {AppId, ChannelName}}),
      % success_presence(ChannelName, erlson:to_json(ErlsonHash))
      % io:format("H: ~p\n", [gsub(jiffy:encode(PresenceHash), "\"", "\\\"")]),
      success_presence(ChannelName, jiffy:encode(PresenceHash))
  end.

unsubscribe(Dict) ->
  {ok, [ChannelName]} = dict:find("channel", Dict),
  {ok, [{AppId, _Key, _Secret, _Name}]} = dict:find("app", Dict),
  gproc:unreg({p, g, {AppId, ChannelName}}),
  success(ChannelName).

extract_user_info(Data) ->
  {List} = jiffy:decode(Data),
  jiffy:encode({remove_user_id(List)}).

remove_user_id(List) ->
  {Data} = jiffy:decode(List),
  remove_user_id(Data, []).

remove_user_id([], Result) ->
  Result;

remove_user_id(List, Result) ->
  [{Key, Value}|T] = List,
  case Key of
    <<"user_id">> ->
      remove_user_id(T, Result);
    _ ->
      remove_user_id(T, [{Key, Value}|Result])
  end.

prepare_channel_info(Presence) ->
  prepare_channel_info(Presence, [], []).

prepare_channel_info([], Ids, Results) ->
  {Ids, {Results}};

prepare_channel_info(Presence, Ids, Results) ->
  [{presence, AppId, ChannelId, Uuid, Info, Pid, CreatedAt}|T] = Presence,
  case lists:any(fun(X) -> Uuid == X end, Ids) of
    true ->
      prepare_channel_info(T, Ids, Results);
    _ ->    
      prepare_channel_info(T, [list_to_binary(integer_to_list(Uuid))|Ids], [{list_to_binary(integer_to_list(Uuid)), {Info}}|Results])
  end.

gsub(Str,Old,New) ->
  RegExp = "\\Q"++Old++"\\E",
  re:replace(Str,RegExp,New,[global, multiline, {return, list}]).

init({ok, SocketId}) ->
  "{\"event\": \"pusher:connection_established\", \"data\": {\"socket_id\": \"" ++ SocketId ++ "\"}}";

init({no_app, AppId}) ->
  "{\"event\":\"pusher:error\",\"data\":{\"code\":4001,\"message\":\"Could not find app by key " ++ binary_to_list(AppId) ++ "\"}}".

ping() ->
  "{\"event\": \"pusher:pong\", \"data\": {}}".

no_user_id(ChannelId) ->
  "{\"event\":\"pusher:error\",\"data\":{\"code\":null,\"message\":\"channel_data must include a user_id when subscribing to presence channels (" ++ binary_to_list(ChannelId) ++ ")\"}}".

success_presence(ChannelId, Data) ->
  jiffy:encode({[{event, <<"pusher_internal:subscription_succeeded">>}, {channel, ChannelId}, {data, Data}]}).
  % "{\"event\": \"pusher_internal:connection_succeedeed\", \"data\":" ++ binary_to_list(Data) ++ ", \"channel\": \"" ++ binary_to_list(ChannelId) ++ "\"}".
  % }

member_added(ChannelId, UserInfo) ->
  "{\"event\":\"pusher_internal:member_added\",\"data\":" ++ binary_to_list(UserInfo) ++ ", \"channel\": \"" ++ binary_to_list(ChannelId) ++ "\"}".

member_removed(ChannelId, UserId) ->
  "".

success(ChannelId) ->
  "{\"event\": \"pusher_internal:subscription_succeeded\", \"data\": {}, \"channel\": \"" ++ binary_to_list(ChannelId) ++ "\"}".