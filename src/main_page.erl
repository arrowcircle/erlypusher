-module(main_page).
-export([init/3, handle/2, terminate/3]).

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [], <<"
<!DOCTYPE html>
<html>
  <head>
    <title>Websocket Test</title>
  </head>
  <body>
    <script src=\"http://js.pusher.com/2.0/pusher.min.js\"></script>
    <script type=\"text/javascript\">
      Pusher.host    = \"127.0.0.1\";
      Pusher.ws_port = \"8081\";
      Pusher.stats_host = '127.0.0.1:8081';
      Pusher.sockjs_host = 'localhost';
      Pusher.sockjs_http_port = '8081';
      Pusher.log = function(data) {
        console.log('\t\t', data);
      };
      var pusher = new Pusher('key1');
      pusher.bind('pusher:error', function(data) { console.log(data.to_json) })
      var myChannel = pusher.subscribe('MY_CHANNEL');
      myChannel.bind('an_event', function(data) { console.log(data) })
    </script>
  </body>
</html>
">>, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
