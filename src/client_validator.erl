-module(client_validator).

-export([check/1]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

check(Dict) ->
  {ok, App} = dict:find("app", Dict),
  case App of
    {_Id, _AppKey, _Secret, _Name} ->
      event(Dict);
    [{_Id, _AppKey, _Secret, _Name}] ->
      event(Dict);
    {no_key, _AppKey} ->
      {error, no_app_by_key};
    _Err ->
      {error, dict_app_error}
  end.

event(Dict) ->
  {ok, EventName} = dict:find("event", Dict),
  case EventName of
    <<"pusher:ping">> ->
      ok;
    <<"pusher:unsubscribe">> ->
      ok;
    <<"pusher:subscribe">> ->
      channel(Dict);
    _ ->
      % check if client events enabled and channel subscribtion
      ok
  end.

channel(Dict) ->
  {ok, ChannelType} = dict:find("channel_type", Dict),
  case ChannelType of
    common ->
      ok;
    private ->
      private(Dict);
    presence ->
      private(Dict);
    _ ->
      {error, channel_error}
  end.

private(Dict) ->
  {ok, {_Id, Key, Secret, _Name}} = dict:find("app", Dict),
  {ok, ChannelName} = dict:find("channel", Dict),
  {ok, Auth} = dict:find("auth", Dict),
  SocketId = gproc:get_value({p, g, socket_id}),
  case authenticator:can_join(ChannelName, SocketId, Auth, "", Secret, Key) of
    ok ->
      ok;
    _ ->
      {error, authentication_error}
  end.

% respond_private_channel(ChannelName, Auth, Req, AppId) ->
%   % ChannelName = request_parser:get_channel_name(Data),
%   {ok, {_Key, Secret, _Name}} = erlypusher_config:app_by_id(AppId),
%   SocketId = gproc:get_value({p, g, socket_id}),
%   {ok, {Key, Secret, _Name}} = erlypusher_config:app_by_id(AppId),
%   case authenticator:can_join(ChannelName, SocketId, Auth, "", Secret, Key) of
%     ok ->
%       gproc:reg({p, g, {AppId, ChannelName}}),
%       json_responder:response({ok_channel, ChannelName});
%     _ ->
%       error
%       % write response for wrong auth
%   end.

% check_channel_type(ChannelName) ->
%   String = binary_to_list(ChannelName),
%   case string:str(String, "presence-") of
%     0 ->
%       case string:str(String, "private-") of
%         0 ->
%           common;
%         _ ->
%           private
%       end;
%     _ ->
%       presence
%   end.