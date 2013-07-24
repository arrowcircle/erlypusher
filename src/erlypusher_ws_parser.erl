-module(erlypusher_ws_parser).

 -export([parse/2]).

-include_lib("erlson/include/erlson.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

channel_type(ChannelName) ->
  String = binary_to_list(ChannelName),
  case string:str(String, "presence-") of
    0 ->
      case string:str(String, "private-") of
        0 ->
          common;
        _ ->
          private
      end;
    _ ->
      presence
  end.

auth(Json) ->
  erlson:get_value(data.auth, Json, undefined).

channel_data(Json) ->
  erlson:get_value(data.channel_data, Json, undefined).

channel(Json) ->
  case catch Json.data.channel of
    ChannelName -> ChannelName;
    _ -> ""
  end.

parse(Req, Data) ->
  FirstDict = dict:new(),
  Json = erlson:from_json(Data),
  {AppKey, Req2} = cowboy_req:binding(key, Req),
  case erlypusher_config:app_by_key(AppKey) of
    {ok, {Id, Secret, Name}} ->
      App = {Id, AppKey, Secret, Name};
    _ ->
      App = {no_key, AppKey}
  end,
  Pid = element(5, Req),
  Event = Json.event,
  case Event of
    <<"pusher:ping">> ->
      EventDict = dict:append("event", Event, FirstDict),
      TypeDict = EventDict;
    _ ->
      Channel = channel(Json),
      ChannelType = channel_type(Channel),
      EventDict = dict:append("event", Event, FirstDict),
      ChannelDict = dict:append("channel", Channel, EventDict),
      TypeDict = dict:append("channel_type", ChannelType, ChannelDict)
  end,
  case auth(Json) of
    undefined -> AuthDict = TypeDict;
    Auth -> AuthDict = dict:append("auth", Auth, TypeDict)
  end,
  case channel_data(Json) of
    undefined ->
      io:format("Data is undefined \n\n"),
      DataDict = AuthDict;
    Data -> 
      io:format("Data is: ~p\n\n", [Data]),
      DataDict = dict:append("data", Data, AuthDict)
  end,
  AppDict = dict:append("app", App, DataDict),
  PidDict = dict:append("pid", Pid, AppDict),
  {PidDict, Req2}.