-module(test_erlypusher_config).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CONFIG_ELEM, [{<<"app_name">>,
   {[{<<"app_id">>,<<"app_ida">>},
     {<<"key">>,<<"keya">>},
     {<<"secret">>,<<"secreta">>}]}}]).

-define(TEST_CONFIG, {[{<<"app_name">>,
   {[{<<"app_id">>,<<"1">>},
     {<<"key">>,<<"12">>},
     {<<"secret">>,<<"13">>}]}},
  {<<"app_name1">>,
   {[{<<"app_id">>,<<"2">>},
     {<<"key">>,<<"22">>},
     {<<"secret">>,<<"23">>}]}}]}).

generate_erlypusher_config_test_() ->
  D = dict:new(),
  D1 = dict:append(<<"app_ida">>, {{<<"keya">>, <<"secreta">>, <<"app_name">>}}, D),
  io:format("dict is ~p\n", [D1]),
  [?_assertEqual(D1, erlypusher_config:parse_config_element(?TEST_CONFIG_ELEM, dict:new()))].