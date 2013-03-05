-module(channels_handler).
-export([init/3, handle/2, terminate/3]).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.

% handle block

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(401, [{<<"content-type">>, <<"application/json">>}], [<<"Unknown auth_key">>], Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
