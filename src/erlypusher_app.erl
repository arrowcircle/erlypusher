-module(erlypusher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cowboy:start_listener(whatever, 5, cowboy_tcp_transport, [{port, 8081}], cowboy_http_protocol,
        [{dispatch,[
                    {'_',[
                           {[<<"app">>, '_'], websocket_handler, []},
                           {[], main_page, []},
                           {[<<"apps">>, app_id, <<"channels">>, channel_id, <<"events">>], api_handler, []}
                         ]}
                   ]}]
    ),
    erlypusher_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
    test_json_responder:test().

-endif.