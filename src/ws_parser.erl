-module(ws_parser).

% -export([get_event/1]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

event(Json) ->
  {[{<<"event">>, Event}, _]} = Json,
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
  {[_|[{<<"data">>, {[_|[{<<"auth">>, Auth}|_]]}}|_]]} = Json,
  Auth.

channel_data(Json) ->
  {[_|[{<<"data">>, {[_|[_|[{<<"channel_data">>, Data}|_]]]}}|_]]} = Json,
  Data.

channel(Json) ->
  {[_|[{<<"data">>, {[{<<"channel">>, Channel_name}|_]}}|_]]} = Json,
  Channel_name.

parse(Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  BodyJson = jiffy:decode(Body),
  {AppKey, Req3} = cowboy_req:binding(key, Req2),
  Dict = ok,
  {Dict, Req}.