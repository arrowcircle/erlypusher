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

load_config(Path) ->
  {ok, Json} = file:read_file(Path),
  case jiffy:decode(Json) of
    {Json} ->
      Json;
    {error, Error} ->
      {error, Error}
  end.

parse_config(Json) ->
  Dict = dict:new(),
  {Arr} = Json,
  parse_config_element(Arr, Dict).

parse_config_element(Config, Dict) ->
  io:format("dict is ~p\n", [Dict]),
  [Elem | NewConfig] = Config,
  case NewConfig of
    [] ->
      Dict;
    [_] ->
      {AppName, {AppInfo}} = Elem,
      [{<<"app_id">>, AppId}|AppKeyAndSecret] = AppInfo,
      [{<<"key">>, Key}|AppSecretTuple] = AppKeyAndSecret,
      [{<<"secret">>, Secret}|_]= AppSecretTuple,
      NewDict = dict:append(AppId, {Key, Secret, AppName}, Dict),
      parse_config_element(NewConfig, NewDict)
  end.
