-module(authenticator_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SIGN_AUTH, <<"1753fe2cb8e3328e791b869f9f3485ebfe3bcc1cda0deec8d9411dda243ed105">>).
-define(SIGN_STRING, "123123-123123-123123-123123:MY_CHANNEL").
-define(SOCKET_ID, "123123-123123-123123-123123").
-define(SECRET, <<"secret_string">>).
-define(SECRET_PARAMS, <<"secret1">>).
-define(SIGNATURE_SIGN_STRING, <<"POST\n/apps/appid1/events\nauth_key=key1&auth_timestamp=1362567231&auth_version=1.0&body_md5=237ac07e2c8f0b6ec7b07fc9ce422d91&channels[]=MY_CHANNEL&data={\"some\":\"Data\"}&name=event1">>).
-define(AUTH_SIGNATURE, <<"362839da642c11542cd92b07b2e94a6cead26572da3117fceb19e72aa6b43771">>).
% -define(UNJOINED_PARAMS, [{<<"auth_key">>, <<"key1">>}, {<<"auth_timestamp">>, <<"1362567231">>}, {<<"auth_version">>, <<"1.0">>}, {<<"body_md5">>, <<"237ac07e2c8f0b6ec7b07fc9ce422d91">>}, {<<"channels">>, [<<"MY_CHANNEL">>]}, {<<"data">>, <<"{\"some\":\"Data\"}">>}, {<<"name">>, <<"event1">>}]).

-define(UNJOINED_PARAMS, [{<<"auth_key">>,<<"key1">>},
                         {<<"auth_timestamp">>,<<"1362567231">>},
                         {<<"auth_version">>,<<"1.0">>},
                         {<<"body_md5">>,<<"237ac07e2c8f0b6ec7b07fc9ce422d91">>},
                         {<<"channels">>,[<<"MY_CHANNEL">>]},
                         {<<"data">>,<<"{\"some\":\"Data\"}">>},
                         {<<"name">>,<<"event1">>}]).

-define(JOINED_PARAMS, <<"auth_key=key1&auth_timestamp=1362567231&auth_version=1.0&body_md5=237ac07e2c8f0b6ec7b07fc9ce422d91&channels[]=MY_CHANNEL&data={\"some\":\"Data\"}&name=event1">>).

generate_authenticator_test_() ->
  [?_assertEqual(?SIGN_AUTH, authenticator:sign(?SIGN_STRING, ?SECRET))].

generate_signature_test_() ->
  [?_assertEqual(?AUTH_SIGNATURE, authenticator:sign(?SIGNATURE_SIGN_STRING, ?SECRET_PARAMS)),
   ?_assertEqual(?SIGNATURE_SIGN_STRING, authenticator:join_signature_string(<<"POST">>, <<"/apps/appid1/events">>, <<"auth_key=key1&auth_timestamp=1362567231&auth_version=1.0&body_md5=237ac07e2c8f0b6ec7b07fc9ce422d91&channels[]=MY_CHANNEL&data={\"some\":\"Data\"}&name=event1">>))].

generate_join_params_test_() ->
  [?_assertEqual(?JOINED_PARAMS, authenticator:format_params(?UNJOINED_PARAMS))].