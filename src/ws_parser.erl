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
  FirstDict = Dict:new();
  Json = erlson:from_json(Body),
  {AppKey, Req3} = cowboy_req:binding(key, Req2),
  {ok, {Id, Secret, Name}} = erlypusher_config:app_by_key(AppKey)
  Event = event(Json),
  Channel = channel(Json),
  ChannelType = channel_type(Channel),
  Auth = auth(Json),
  Data = channel_data(Json),
  EventDict = Dict:append("event", Event, FirstDict),
  ChannelDict = Dict:append("channel", Channel, EventDict),
  TypeDict = Dict:append("channel_type", ChannelType, ChannelDict),
  AuthDict = Dict:append("auth", Auth, TypeDict),
  DataDict = Dict:append("data", Data, AuthDict),
  AppDict = Dict:append("app", {Id, AppKey, Secret, Name}, DataDict),
  {AppDict, Req}.