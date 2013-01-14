-module(erlypusher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cowboy:start_listener(whatever, 5, cowboy_tcp_transport, [{port, 8081}], cowboy_http_protocol,
        [{dispatch,[
                    {'_',[
                           {[<<"app">>, app_id], websocket_handler, []},
                           {[], main_page, []}
                         ]}
                   ]}]
    ),
    erlypusher_sup:start_link().

stop(_State) ->
    ok.
