-module(main_page).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, Req2} = cowboy_http_req:reply(200, [], <<"<html>
<head>
<script type=\"text/javascript\">
var ws;
function addStatus(text){
  var date = new Date();
  document.getElementById('status').innerHTML
    = document.getElementById('status').innerHTML
    + date + \": \" + text + \"<br/>\";
}
function ready(){
  if (\"MozWebSocket\" in window) {
    WebSocket = MozWebSocket;
  }
  if (\"WebSocket\" in window) {
    // browser supports websockets
                ws = new WebSocket(\"ws://localhost:8080/websocket\");
    ws.onopen = function() {
      // websocket is connected
      addStatus(\"websocket connected!\");
      // send hello data to server.
      ws.send(\"hello server!\");
      addStatus(\"sent message to server: 'hello server'!\");
    };
    ws.onmessage = function (evt) {
      var receivedMsg = evt.data;
      addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
    };
    ws.onclose = function() {
      // websocket was closed
      addStatus(\"websocket was closed\");
    };
  } else {
    // browser does not support websockets
    addStatus(\"sorry, your browser does not support websockets.\");
  }
}
</script>
</head>
<body onload=\"ready();\">
<form name=\"chat\" onsubmit=\"ws.send(document.chat.msg.value); return false;\">
<input name=\"msg\" type=\"text\"/>
<input type=\"submit\"/>
</form>
<div id=\"status\"></div>
</body>
</html>">>, Req),
  {ok, Req2, State}.


terminate(_Req, _State) ->
  ok.
