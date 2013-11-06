-module(bigwig).
-export([start/0, stop/0]).

start() ->
    application:ensure_started(ranch),
    application:ensure_started(jsx),
    
    application:ensure_started(crypto),
    application:ensure_started(sasl),
    application:ensure_started(cowlib),
    application:ensure_started(cowboy),
    application:start(bigwig).

stop() ->
    application:stop(bigwig).
