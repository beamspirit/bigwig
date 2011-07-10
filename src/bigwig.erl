-module(bigwig).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    ensure_started(crypto),
    ensure_started(sasl),
    ensure_started(cowboy),
    application:start(bigwig).

stop() ->
    application:stop(bigwig).
