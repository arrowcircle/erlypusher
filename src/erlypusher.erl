%% Feel free to use, reuse and abuse the code in this file.

-module(erlypusher).

%% API.
-export([start/0]).

%% API.

start() ->
    net_adm:world(),
    erlypusher_config:prepare(),
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    application:start(gproc),
    application:start(erlypusher).