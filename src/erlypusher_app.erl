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
                           {[<<"app">>, '_'], websocket_handler, []},
                           {[], main_page, []},
                           {[<<"apps">>, '_', <<"channels">>, '_', <<"events">>], api_handler, []}
                         ]}
                   ]}]
    ),
    erlypusher_sup:start_link().

stop(_State) ->
    ok.
