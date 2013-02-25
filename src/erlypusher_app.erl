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
    {ok, [[Port|_]|_]} = init:get_argument(port),
    Dispatch = cowboy_router:compile([
                    %% {HostMatch, list({PathMatch, Handler, Opts})}
                    {'_', [{"/app/:key", websocket_handler, []},
                           {"/", main_page, []},
                           {"/apps/:app_id/channels/:channel_id/events", api_handler, []}
                          ]}
                ]),
    cowboy:start_http(my_http_listener, 100,
        [{port, list_to_integer(Port)}],
        [{env, [{dispatch, Dispatch}]},
         {middlewares, [cowboy_router, middleware, cowboy_handler]}
        ]
    ),
    erlypusher_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).

simple_test() ->
    test_json_responder:test().

-endif.