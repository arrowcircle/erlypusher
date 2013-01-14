%% Feel free to use, reuse and abuse the code in this file.

-module(erlypusher).

%% API.
-export([start/0]).

%% API.

start() ->
    application:start(cowboy),
    application:start(gproc),
    application:start(erlypusher).