-module(ws_parser).

 -export([parse/2]).

-include_lib("erlson/include/erlson.hrl").

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

event(Json) ->
  Event = Json.event,
  Event.

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
  case catch Json.data.auth of
    Auth ->
      Auth;
    _ ->
      error
  end.

channel_data(Json) ->
  case catch Json.data.channel_data of
    ChannelData -> ChannelData;
    _ -> {}
  end.

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
  Event = event(Json),
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
    Auth -> AuthDict = dict:append("auth", Auth, TypeDict);
    error -> AuthDict = TypeDict
  end,
  case catch channel_data(Json) of
    Data -> DataDict = dict:append("data", Data, AuthDict);
    _ -> DataDict = AuthDict
  end,
  AppDict = dict:append("app", App, DataDict),
  PidDict = dict:append("pid", Pid, AppDict),
  {PidDict, Req2}.