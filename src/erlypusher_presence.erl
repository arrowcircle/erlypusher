-module(erlypusher_presence).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([subscribe/5, start_link/0]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(presence, {app_id, channel_id, uuid, info, pid, created_at}).

%% Public API

subscribe(AppId, ChannelId, UserInfo, Pid, Uuid) ->
  gen_server:call({global, ?MODULE}, {subscribe, AppId, ChannelId, UserInfo, Pid, Uuid}).

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call({global, ?MODULE}, stop).

state() ->
  gen_server:call({global, ?MODULE}, state).


%% Server implementation

init([]) ->
  init_presence(),
  {ok, []}.

handle_call({subscribe, AppId, ChannelId, UserInfo, Pid, Uuid}, _From, State) ->
  subscribe_user(AppId, ChannelId, UserInfo, Pid, Uuid),
  {reply, ok, State};

handle_call({list, ChannelId, AppId}, _From, State) ->
  io:format("\n~p: Received list event: ~p\n", [AppId, ChannelId]),
  {reply, ok, State};

handle_call({unsubscribe, UserId, AppId}, _From, State) ->
  io:format("\n~p: Received unsubscribe event: ~p\n", [AppId, UserId]),
  {reply, ok, State};

handle_call(stop, _From, State) ->
  mnesia:stop(),
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  {reply, ignored_message, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions

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
          {ram_copies, [node()]}
      ])
  end.

unsubscribe_user(AppId, ChannelId, Uuid) ->
  {atomic, Presences} = get_user_info(AppId, ChannelId, Uuid),
  F = fun() ->
    lists:foreach(fun(Presence) -> mnesia:delete_object(Presence) end, Presences) end,
  mnesia:transaction(F).

get_user_info(AppId, ChannelId, Uuid) ->
  F = fun() ->
    Query = qlc:q([M || M <- mnesia:table(presence),
      M#presence.app_id =:= AppId,
      M#presence.channel_id =:= ChannelId,
      M#presence.uuid =:= Uuid]),
    Results = qlc:e(Query) end,
  mnesia:transaction(F).

subscribe_user(AppId, ChannelId, UserInfo, Pid, Uuid) ->
  case get_user_info(AppId, ChannelId, Uuid) of
    {atomic,[]} ->
      F = fun() ->
        {_, CreatedAt, _} = erlang:now(),
        mnesia:write(#presence{app_id=AppId, channel_id=ChannelId, info=UserInfo, pid=Pid, uuid=Uuid, created_at=CreatedAt})
      end,
      mnesia:transaction(F);
    _ ->
      ok
  end.
