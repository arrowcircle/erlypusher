-module(erlypusher_config).
-compile(export_all).

-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

set({AppIds, AppKeys}) ->
  application:set_env(erlypusher, app_ids, AppIds),
  application:set_env(erlypusher, app_keys, AppKeys),
  ok.

get() ->
  case application:get_env(erlypusher, config) of
    {ok, Config} -> Config;
    undefined -> []
  end.

prepare() ->
  [AppIds, AppKeys] = parse(load(lookup())),
  set({AppIds, AppKeys}).

lookup() ->
  case os:getenv("ERLYPUSHER") of
    false ->
      ConfigPaths = [".", "/etc/erlypusher"],
      case file:path_open(ConfigPaths, "erlypusher.conf", [raw, read, read_ahead]) of
        {ok, _, Path} ->
          {ok, Json} = file:read_file(Path),
          Json;
        {error, Error} ->
          {error, Error}
      end;
    Path ->
      case file:read_file(Path) of
        {ok, Json} ->
          Json;
        {error, Error} ->
          {error, Error}
      end
  end.

load(File) ->
  case jiffy:decode(File) of
    {Json} ->
      Json;
    {error, Error} ->
      {error, Error}
  end.

parse(Json) ->
  AppIds = dict:new(),
  AppKeys = dict:new(),
  parse_element(Json, AppIds, AppKeys).

parse_info(El) ->
  {AppName, InfoHash} = El,
  {[{<<"app_id">>, AppId}|KeySecretArr]} = InfoHash,
  [{<<"key">>, Key}|SecretArr] = KeySecretArr,
  [{<<"secret">>, Secret}]= SecretArr,
  {AppId, Key, Secret, AppName}.

parse_element(Config, AppIds, AppKeys) ->
  case Config of
    [] ->
      [AppIds, AppKeys];
    [Elem | NewConfig] ->
      [Elem | NewConfig] = Config,
      {AppId, Key, Secret, AppName} = parse_info(Elem),
      NewAppIds = dict:store(AppId, {Key, Secret, AppName}, AppIds),
      NewAppKeys = dict:store(Key, {AppId, Secret, AppName}, AppKeys),
      parse_element(NewConfig, NewAppIds, NewAppKeys);
    {Array} ->
      parse_element(Array, AppIds, AppKeys);
    _ ->
      {AppId, Key, Secret, AppName} = parse_info(Config),
      NewAppIds = dict:store(AppId, {Key, Secret, AppName}, AppIds),
      NewAppKeys = dict:store(Key, {AppId, Secret, AppName}, AppKeys),
      [NewAppIds, NewAppKeys]
  end.