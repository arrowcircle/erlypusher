-module(middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  [{handler, Handler}|_] = Env,
  case Handler of
    websocket_handler ->
      {ok, Req, Env};
    api_handler ->
      {AppId, Req2} = cowboy_req:binding(app_id, Req),
      case erlypusher_config:app_by_id(AppId) of
        error ->
          {error, 404, Req2};
        {ok, _} ->
          {ok, Req2, Env}
      end;
    main_page ->
      {ok, Req, Env}
  end.
