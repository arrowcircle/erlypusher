-module(test_authenticator).

-include_lib("eunit/include/eunit.hrl").

-define(SIGN_AUTH, "1753fe2cb8e3328e791b869f9f3485ebfe3bcc1cda0deec8d9411dda243ed105").
-define(SIGN_STRING, "123123-123123-123123-123123:MY_CHANNEL").
-define(SOCKET_ID, "123123-123123-123123-123123").
-define(SECRET, <<"secret_string">>).

generate_authenticator_test_() ->
  [?_assertEqual(?SIGN_AUTH, authenticator:sign(?SIGN_STRING, ?SECRET))].