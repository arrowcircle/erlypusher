-module(erlypusher_config).

-export([prepare/0, app_by_id/1, app_by_key/1, readlines/1]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

app_by_id(Id) ->
  Env = application:get_env(erlypusher, app_ids),
  case Env of
    {ok, Dict} ->
      dict:find(Id, Dict);
    undefined ->
      error
  end.

app_by_key(Key) ->
  {ok, Dict} = application:get_env(erlypusher, app_keys),
  dict:find(Key, Dict).

set({AppIds, AppKeys}, Port) ->
  application:set_env(erlypusher, app_ids, AppIds),
  application:set_env(erlypusher, app_keys, AppKeys),
  application:set_env(erlypusher, port, Port),
  ok.

prepare() ->
  {{AppIds, AppKeys}, Port} = parse(load(lookup())),
  set({AppIds, AppKeys}, Port).

readlines(FileName) ->
  {ok, Content} = file:read_file(FileName),
  Content.

lookup() ->
  case os:getenv("ERLYPUSHER") of
    false ->
      ConfigPaths = [".", "/etc/erlypusher"],
      case file:path_open(ConfigPaths, "erlypusher.conf", [raw, read, binary]) of
        {ok, _Device, Path} ->
          readlines(Path);
        {error, Error} ->
          {error, Error}
      end;
    Path ->
      readlines(Path)
  end.

extract_port(Json) ->
  case Json of
    {[{<<"port">>, Port}|_]} ->
      Port;
    [{<<"port">>, Port}|_] ->
      Port
  end.

extract_apps(Json) ->
  case Json of
    {[_|[{<<"apps">>, AppsJson}|_]]} ->
      AppsJson;
    [_|[{<<"apps">>, AppsJson}|_]] ->
      AppsJson
  end.

load(File) ->
  case jiffy:decode(File) of
    {Json} ->
      Json;
    {error, Error} ->
      {error, Error}
  end.

parse(Json) ->
  Port = extract_port(Json),
  {parse_apps_array(extract_apps(Json), dict:new(), dict:new()), Port}.

parse_info(El) ->
  case helper:type_of(El) of
    list ->
      [{AppName, InfoHash}|_] = El;
    tuple ->
      case El of
        {[{AppName, InfoHash}]} ->
          ok;
        {AppName, InfoHash} ->
          ok
      end
  end,
  {[{<<"app_id">>, AppId}|KeySecretArr]} = InfoHash,
  [{<<"key">>, Key}|SecretArr] = KeySecretArr,
  [{<<"secret">>, Secret}]= SecretArr,
  {AppId, Key, Secret, AppName}.

parse_apps_array(AppsArray, AppIds, AppKeys) ->
  case AppsArray of
    [] ->
      {AppIds, AppKeys};
    [Elem | NewAppsArray] ->
      % [Elem | NewAppsArray] = AppsArray,
      {AppId, Key, Secret, AppName} = parse_info(Elem),
      parse_apps_array(NewAppsArray, dict:store(AppId, {Key, Secret, AppName}, AppIds), dict:store(Key, {AppId, Secret, AppName}, AppKeys))
  end.