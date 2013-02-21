-module(test_erlypusher_config).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CONFIG_ELEM, {<<"appname">>,
   {[{<<"app_id">>, <<"app1">>},
     {<<"key">>, <<"key1">>},
     {<<"secret">>, <<"secret1">>}]}}).

-define(TEST_CONFIG_ELEM_RES, {<<"app1">>, <<"key1">>, <<"secret1">>, <<"appname">>}).

-define(TEST_CONFIG_COMPLEX, {[{<<"appname1">>,
   {[{<<"app_id">>, <<"app1">>},
     {<<"key">>, <<"key1">>},
     {<<"secret">>, <<"secret1">>}]}},
  {<<"appname2">>,
   {[{<<"app_id">>, <<"app2">>},
     {<<"key">>, <<"key2">>},
     {<<"secret">>, <<"secret2">>}]}}]}).

-define(TEST_CONFIG_SIMPLE, {<<"appname">>,
   {[{<<"app_id">>, <<"app1">>},
     {<<"key">>, <<"key1">>},
     {<<"secret">>, <<"secret1">>}]}}).

-define(TEST_CONFIG_SIMPLE_RES, gen_test_config_simple_res()).

-define(TEST_CONFIG_COMPLES_RES, gen_test_config_complex_res()).

gen_test_config_simple_res() ->
  D = dict:new(),
  Dd = dict:new(),
  D1 = dict:store(<<"app1">>, {<<"key1">>, <<"secret1">>, <<"appname">>}, D),
  Dd1 = dict:store(<<"key1">>, {<<"app1">>, <<"secret1">>, <<"appname">>}, Dd),
  [D1, Dd1].

gen_test_config_complex_res() ->
  D = dict:new(),
  Dd = dict:new(),
  D1 = dict:store(<<"app1">>, {<<"key1">>, <<"secret1">>, <<"appname1">>}, D),
  Dd1 = dict:store(<<"key1">>, {<<"app1">>, <<"secret1">>, <<"appname1">>}, Dd),
  D2 = dict:store(<<"app2">>, {<<"key2">>, <<"secret2">>, <<"appname2">>}, D1),
  Dd2 = dict:store(<<"key2">>, {<<"app2">>, <<"secret2">>, <<"appname2">>}, Dd1),
  [D2, Dd2].

generate_erlypusher_config_test_() ->
  [?_assertEqual(?TEST_CONFIG_ELEM_RES, erlypusher_config:parse_info(?TEST_CONFIG_ELEM)),
   ?_assertEqual(?TEST_CONFIG_SIMPLE_RES, erlypusher_config:parse(?TEST_CONFIG_SIMPLE)),
   ?_assertEqual(?TEST_CONFIG_COMPLES_RES, erlypusher_config:parse(?TEST_CONFIG_COMPLEX))].