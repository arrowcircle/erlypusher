-module(erlypusher_config).
-compile(export_all).

-export([]).

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
      case file:path_consult(ConfigPaths, "erlypusher.conf") of
        {ok, Env1, ConfigPath} ->
          {ok, Env1, ConfigPath};
        {error, Error} ->
          {error, Error}
      end;
    Path ->
      case file:consult(Path) of
        {ok, Env} ->
          {ok, Env, Path};
        {error, Error} ->
          {error, Error}
      end
  end.

load_config(Path) ->
  {ok, Json} = file:read_file(Path),
  jiffy:decode(Json).

parse_config(Json) ->
  Dict = dict:new().
  % parse_config_element(Json, Dict).

% parse_config_element(Config, Dict) ->
%   [Elem | NewConfig] = Config,
%   pase_config_element(NewConfig, dict:append(Dict, Elem)).