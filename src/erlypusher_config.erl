-module(erlypusher_config).

-export([prepare/0, app_by_id/1, app_by_key/1, readlines/1]).

-include_lib("erlson/include/erlson.hrl").

app_by_id(Id) ->
  Env = application:get_env(erlypusher, app_ids),
  case Env of
    {ok, Dict} ->
      orddict:find(Id, Dict);
    undefined ->
      error
  end.

app_by_key(Key) ->
  case application:get_env(erlypusher, app_keys) of
  {ok, Dict} ->
    orddict:find(Key, Dict);
  _ ->
    {error, app_not_found}
  end.

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

extract_apps(Json) ->
  Apps = Json.apps,
  Apps.

load(File) ->
  erlson:from_json(File).

parse(Json) ->
  Port = Json.port,
  {parse_apps_array(extract_apps(Json), orddict:new(), orddict:new()), Port}.

parse_apps_array(AppsArray, AppIds, AppKeys) ->
  case AppsArray of
    [] ->
      {AppIds, AppKeys};
    [Elem | NewAppsArray] ->
      parse_apps_array(NewAppsArray, orddict:store(Elem.app_id, {Elem.key, Elem.secret, Elem.name}, AppIds), orddict:store(Elem.key, {Elem.app_id, Elem.secret, Elem.name}, AppKeys))
  end.