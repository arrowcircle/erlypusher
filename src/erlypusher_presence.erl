-module(erlypusher_presence).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([store/0, start/0]).
-compile(export_all).

-record(presence, {app_id, channel_id, uuid, info, created_at, pid}).

%% Public API

store() ->
  gen_server:call(?MODULE, {msg}).

start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

state() ->
  gen_server:call(?MODULE, state).


%% Server implementation

init([]) ->
  init_presence(),
  {ok, []}.

handle_call({subscribe, UserInfo, AppId}, _From, State) ->
  io:format("\n~p: Received subscribtion event!: ~p\n", [AppId, UserInfo]),
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
          {ram_copies, node()}
      ])
  end.