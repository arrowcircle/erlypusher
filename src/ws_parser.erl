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
  % get body and convert to json
  {ok, Body, Req2} = cowboy_req:body(Req),
  Json = erlson:from_json(Body),
  % get app_key and return app
  {AppKey, Req3} = cowboy_req:binding(key, Req2),
  % get event
  Event = event(Json),
  % if event is subscription, get channel and channel_type
  Channel = channel(Json),
  ChannelType = channel_type(Channel),
  % if channel type is private or presence - get auth
  Auth = auth(Json),
  % if channel type is presence - get channel_data
  Data = channel_data(Json),
  Dict = ok,
  {Dict, Req}.