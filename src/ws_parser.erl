-module(ws_parser).

% -export([get_event/1]).

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
  Auth = Json.data.auth,
  Auth.

channel_data(Json) ->
  Data = Json.data.channel_data,
  Data.

channel(Json) ->
  Channel_name = Json.data.channel,
  Channel_name.

parse(Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  FirstDict = dict:new(),
  Json = erlson:from_json(Body),
  {AppKey, Req3} = cowboy_req:binding(key, Req2),
  case erlypusher_config:app_by_key(AppKey) of
    {ok, {Id, Secret, Name}} ->
      App = {Id, AppKey, Secret, Name};
    _ ->
      App = {no_key, AppKey}
  end,
  Event = event(Json),
  Channel = channel(Json),
  ChannelType = channel_type(Channel),
  Auth = auth(Json),
  Data = channel_data(Json),
  EventDict = dict:append("event", Event, FirstDict),
  ChannelDict = dict:append("channel", Channel, EventDict),
  TypeDict = dict:append("channel_type", ChannelType, ChannelDict),
  AuthDict = dict:append("auth", Auth, TypeDict),
  DataDict = dict:append("data", Data, AuthDict),
  AppDict = dict:append("app", App, DataDict),
  {AppDict, Req}.