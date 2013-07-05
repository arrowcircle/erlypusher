-module(erlypusher_api_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OK_REQUEST, {http_req,2684,ranch_tcp,close,286,<<"POST">>,
          {1,1},
          {{127,0,0,1},59014},
          <<"localhost">>,undefined,8081,<<"/apps/appid1/events">>,undefined,
          <<"body_md5=f526bda6bdd083b269a52680132c1e4c&auth_version=1.0&auth_key=key1&auth_timestamp=1362069507&auth_signature=407270f69e7742af7edb0bf0070bd80b64a76fe811e228eaeac20dac2f18f90c">>,
          undefined,<<>>,
          [{app_id,<<"appid1">>}],
          [{<<"content-type">>,<<"application/json">>},
           {<<"accept">>,<<"*/*">>},
           {<<"user-agent">>,<<"Ruby">>},
           {<<"connection">>,<<"close">>},
           {<<"host">>,<<"localhost:8081">>},
           {<<"content-length">>,<<"73">>}],
          [{<<"connection">>,[<<"close">>]}],
          undefined,[],waiting,undefined,
          <<"{\"name\":\"an_event\",\"channels\":[\"a_channel\"],\"data\":\"{\\\"some\\\":\\\"data\\\"}\"}">>,
          false,waiting,[],<<>>,undefined}).

-define(NOAPP_REQUEST, {http_req,2684,ranch_tcp,close,286,<<"POST">>,
          {1,1},
          {{127,0,0,1},59014},
          <<"localhost">>,undefined,8081,<<"/apps/appid3/events">>,undefined,
          <<"body_md5=f526bda6bdd083b269a52680132c1e4c&auth_version=1.0&auth_key=key1&auth_timestamp=1362069507&auth_signature=407270f69e7742af7edb0bf0070bd80b64a76fe811e228eaeac20dac2f18f90c">>,
          undefined,<<>>,
          [{app_id,<<"appid1">>}],
          [{<<"content-type">>,<<"application/json">>},
           {<<"accept">>,<<"*/*">>},
           {<<"user-agent">>,<<"Ruby">>},
           {<<"connection">>,<<"close">>},
           {<<"host">>,<<"localhost:8081">>},
           {<<"content-length">>,<<"73">>}],
          [{<<"connection">>,[<<"close">>]}],
          undefined,[],waiting,undefined,
          <<"{\"name\":\"an_event\",\"channels\":[\"a_channel\"],\"data\":\"{\\\"some\\\":\\\"data\\\"}\"}">>,
          false,waiting,[],<<>>,undefined}).

-define(OK_RESPONSE, ok).

-define(NOAPP_RESPONSE, ok).

% app_ids() ->
%   dict:store(<<"app1">>, {<<"key1">>, <<"secret1">>, <<"appname">>}, dict:new()).

% app_keys() ->
%   dict:store(<<"key1">>, {<<"app1">>, <<"secret1">>, <<"appname">>}, dict:new()).

setup() ->
  ok.
  % erlypusher_config:set({app_ids(), app_keys()}).

clean() ->
  ok.
  % meck:unload(erlypusher_config).

api_handler_test_() ->
  [].

generate_api_handler_test_() ->
  [{setup, fun() -> setup() end, fun(_) -> clean() end, fun api_handler_test_/0}].


% [?_assertEqual(api_handler:handle(?OK_REQUEST, undefined), ?OK_RESPONSE), ?_assertEqual(api_handler:handle(?OK_REQUEST, undefined), ?NOAPP_RESPONSE)].

