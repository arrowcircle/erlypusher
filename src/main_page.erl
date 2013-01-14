-module(main_page).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  io:format("Got main_page handle line 8~n"),
  {ok, Req2} = cowboy_http_req:reply(200, [], <<"
<!DOCTYPE html>
<html>
  <head>
    <title>Websocket Test</title>
  </head>
  <body>
    <script src=\"http://js.pusherapp.com/1.8/pusher.min.js\"></script>
    <script type=\"text/javascript\">
      Pusher.host    = \"127.0.0.1\"
      Pusher.ws_port = \"8080\"
      Pusher.log = function(data) {
        console.log('\t\t', data);
      };
      var pusher = new Pusher('765ec374ae0a69f4ce44');
      pusher.bind('pusher:error', function(data) { console.log(data.to_json) })
      var myChannel = pusher.subscribe('MY_CHANNEL');
      myChannel.bind('an_event', function(data) { console.log(data) })
    </script>
  </body>
</html>
">>, Req),
  {ok, Req2, State}.


terminate(_Req, _State) ->
  ok.
