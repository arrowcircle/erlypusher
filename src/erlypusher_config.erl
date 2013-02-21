-module(erlypusher_config).
-compile(export_all).

-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

set_config(Config) ->
  application:set_env(erlypusher, config, Config).

get_config() ->
  case application:get_env(erlypusher, config) of
    {ok, Config} -> Config;
    undefined -> []
  end.

prepare_config() ->
  parse_config(load_config(lookup_config())).

lookup_config() ->
  case os:getenv("ERLYPUSHER_CONFIG") of
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

load_config(File) ->
  case jiffy:decode(File) of
    {Json} ->
      Json;
    {error, Error} ->
      {error, Error}
  end.

parse_config(Json) ->
  AppIds = Dict = dict:new(),
  AppKeys = Dict = dict:new(),
  parse_config_element(Json, AppIds, AppKeys).

parse_info(El) ->
  {AppName, InfoHash} = El,
  {[{<<"app_id">>, AppId}|KeySecretArr]} = InfoHash,
  [{<<"key">>, Key}|SecretArr] = KeySecretArr,
  [{<<"secret">>, Secret}]= SecretArr,
  {AppId, Key, Secret, AppName}.

parse_config_element(Config, AppIds, AppKeys) ->
  case Config of
    [] ->
      [AppIds, AppKeys];
    [Elem | NewConfig] ->
      [Elem | NewConfig] = Config,
      io:format("\n\n~p\n~p\n", [Config, NewConfig]),
      {AppId, Key, Secret, AppName} = parse_info(Elem),
      NewAppIds = dict:store(AppId, {Key, Secret, AppName}, AppIds),
      NewAppKeys = dict:store(Key, {AppId, Secret, AppName}, AppKeys),
      parse_config_element(NewConfig, NewAppIds, NewAppKeys);
    {Array} ->
      parse_config_element(Array, AppIds, AppKeys);
    _ ->
      {AppId, Key, Secret, AppName} = parse_info(Config),
      NewAppIds = dict:store(AppId, {Key, Secret, AppName}, AppIds),
      NewAppKeys = dict:store(Key, {AppId, Secret, AppName}, AppKeys),
      [NewAppIds, NewAppKeys]
  end.