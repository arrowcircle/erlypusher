-module(erlypusher_config_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CONFIG_ELEM, [{<<"appname">>,
   {[{<<"app_id">>, <<"app1">>},
     {<<"key">>, <<"key1">>},
     {<<"secret">>, <<"secret1">>}]}}]).

-define(TEST_CONFIG_ELEM_RES, {<<"app1">>, <<"key1">>, <<"secret1">>, <<"appname">>}).

-define(TEST_CONFIG_COMPLEX, [{[{<<"app_name1">>,
              {[{<<"app_id">>,<<"appid1">>},
                {<<"key">>,<<"key1">>},
                {<<"secret">>,<<"secret1">>}]}}]},
           {[{<<"app_name2">>,
              {[{<<"app_id">>,<<"appid2">>},
                {<<"key">>,<<"key2">>},
                {<<"secret">>,<<"secret2">>}]}}]}]).

-define(TEST_CONFIG_SIMPLE, {<<"appname">>,
   {[{<<"app_id">>, <<"app1">>},
     {<<"key">>, <<"key1">>},
     {<<"secret">>, <<"secret1">>}]}}).

-define(TEST_CONFIG_JSON, <<"{\n  \"port\": 8080,\n  \"apps\": [\n    {\"app_name1\": {\n      \"app_id\": \"appid1\",\n      \"key\": \"key1\",\n      \"secret\": \"secret1\"\n    }},\n    {\"app_name2\": {\n      \"app_id\": \"appid2\",\n      \"key\": \"key2\",\n      \"secret\": \"secret2\"\n    }}\n  ]\n}">>).

-define(TEST_CONFIG_SIMPLE_RES, gen_test_config_simple_res()).

-define(TEST_CONFIG_COMPLES_RES, gen_test_config_complex_res()).

gen_test_config_simple_res() ->
  D = dict:new(),
  Dd = dict:new(),
  D1 = dict:store(<<"app1">>, {<<"key1">>, <<"secret1">>, <<"appname">>}, D),
  Dd1 = dict:store(<<"key1">>, {<<"app1">>, <<"secret1">>, <<"appname">>}, Dd),
  {D1, Dd1}.

gen_test_config_complex_res() ->
  D = dict:new(),
  Dd = dict:new(),
  D1 = dict:store(<<"appid1">>, {<<"key1">>, <<"secret1">>, <<"app_name1">>}, D),
  Dd1 = dict:store(<<"key1">>, {<<"appid1">>, <<"secret1">>, <<"app_name1">>}, Dd),
  D2 = dict:store(<<"appid2">>, {<<"key2">>, <<"secret2">>, <<"app_name2">>}, D1),
  Dd2 = dict:store(<<"key2">>, {<<"appid2">>, <<"secret2">>, <<"app_name2">>}, Dd1),
  {D2, Dd2}.

generate_erlypusher_config_test_() ->
  [?_assertEqual(?TEST_CONFIG_ELEM_RES, erlypusher_config:parse_info(?TEST_CONFIG_ELEM)),
   ?_assertEqual(?TEST_CONFIG_SIMPLE_RES, erlypusher_config:parse_apps_array(?TEST_CONFIG_ELEM, dict:new(), dict:new())),
   ?_assertEqual(?TEST_CONFIG_COMPLES_RES, erlypusher_config:parse_apps_array(?TEST_CONFIG_COMPLEX, dict:new(), dict:new())),
   ?_assertEqual(8080, erlypusher_config:extract_port(jiffy:decode(?TEST_CONFIG_JSON))),
   ?_assertEqual(?TEST_CONFIG_COMPLEX, erlypusher_config:extract_apps(jiffy:decode(?TEST_CONFIG_JSON)))].