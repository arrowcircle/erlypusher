-module(erlypusher_timeline_handler).
-export([init/3, handle/2, terminate/3]).

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/javascript">>}], <<"Pusher.JSONP.receive(1, null, {});">>, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
