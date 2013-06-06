-module(ws_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlson/include/erlson.hrl").
-define(COMMON_CHANNEL_JSON, erlson:from_json(<<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"MY_CHANNEL\"}}">>)).
-define(AUTH_JSON, erlson:from_json(<<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"MY_CHANNEL\", \"auth\": \"AUTHKEY\"}}">>)).
-define(DATA_JSON, erlson:from_json(<<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"presence-example-channel\", \"auth\": \"AUTHKEY\", \"channel_data\": \"CHANNELOBJECT\"}}">>)).

-define(COMMON_CHANNEL, <<"MY_CHANNEL">>).
-define(PRIVATE_CHANNEL, <<"private-MY_CHANNEL">>).
-define(PRESENCE_CHANNEL, <<"presence-MY_CHANNEL">>).

-define(REQ, {http_req,3518,ranch_tcp,keepalive,
                                      289,<<"GET">>,
                                      {1,1},
                                      {{127,0,0,1},55512},
                                      <<"127.0.0.1">>,undefined,8081,
                                      <<"/app/key1">>,undefined,
                                      <<"protocol=5&client=js&version=1.12.7&flash=false">>,
                                      undefined,<<>>,
                                      [{key,<<"key1">>}],
                                      [{<<"upgrade">>,<<"websocket">>},
                                       {<<"connection">>,<<"Upgrade">>},
                                       {<<"host">>,<<"127.0.0.1:8081">>},
                                       {<<"origin">>,
                                        <<"http://localhost:3000">>},
                                       {<<"pragma">>,<<"no-cache">>},
                                       {<<"cache-control">>,<<"no-cache">>},
                                       {<<"sec-websocket-key">>,
                                        <<"d1CT/4Fk/O8dq2luPKEfoA==">>},
                                       {<<"sec-websocket-version">>,<<"13">>},
                                       {<<"sec-websocket-extensions">>,
                                        <<"x-webkit-deflate-frame">>}],
                                      [{<<"upgrade">>,[<<"websocket">>]},
                                       {<<"connection">>,[<<"upgrade">>]}],
                                      undefined,
                                      [{websocket_version,13}],
                                      waiting,undefined,<<>>,false,done,[],
                                      <<>>,undefined}).
-define(DATA, <<"{\"event\":\"pusher:subscribe\",\"data\":{\"channel\":\"private-MY_CHANNEL\",\"auth\":\"key1:45823142681eab5288403e4168f7781e6e177e33f2656c17b2b92216af12b886\"}}">>).
% -define(AUTH, auth(ws_parser:parse(?REQ, ?DATA))).

auth({Dict, Req}) ->
  {ok, Auth} = dict:find("auth", Dict),
  Auth.

generate_request_parser_test_() ->
   {setup,
    fun () -> erlson:init() end,
    [?_assertEqual(<<"pusher:subscribe">>, ws_parser:event(?COMMON_CHANNEL_JSON)),
     ?_assertEqual(<<"MY_CHANNEL">>, ws_parser:channel(?COMMON_CHANNEL_JSON)),
     ?_assertEqual(common, ws_parser:channel_type(?COMMON_CHANNEL)),
     ?_assertEqual(private, ws_parser:channel_type(?PRIVATE_CHANNEL)),
     ?_assertEqual(presence, ws_parser:channel_type(?PRESENCE_CHANNEL)),
     ?_assertEqual(<<"AUTHKEY">>, ws_parser:auth(?AUTH_JSON)),
     ?_assertEqual(<<"CHANNELOBJECT">>, ws_parser:channel_data(?DATA_JSON))
     % ?_assertEqual([<<"key1:45823142681eab5288403e4168f7781e6e177e33f2656c17b2b92216af12b886">>], ?AUTH)
     ]
   }.