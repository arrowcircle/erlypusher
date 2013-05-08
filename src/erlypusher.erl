%% Feel free to use, reuse and abuse the code in this file.

-module(erlypusher).

-include_lib("erlson/include/erlson.hrl").
%% API.
-export([start/0]).

%% API.

start() ->
    net_adm:world(),
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    application:start(gproc),
    application:start(erlypusher).