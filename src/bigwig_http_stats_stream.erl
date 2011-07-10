%% Websocket connection that sends periodic stats about the VM
%%
-module(bigwig_http_stats_stream).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3]).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

handle(_Req, _State) ->
    exit(websockets_only).

terminate(_Req, _State) ->
    exit(websockets_only).

websocket_init(_TransportName, Req, _Opts) ->
    bigwig_pubsubhub:register_client(self()),
    {ok, Req, undefined_state}.

%% TODO handle stuff like {bigwig, {appmon, ... }} and send that too

websocket_handle({bigwig, {bigwig_stats_sender, Stats}}, Req, State) ->
    Reply = jsx:term_to_json([{stats, Stats}]),
    {reply, Reply, Req, State};

websocket_handle({bigwig, {bigwig_appmon, Stats}}, Req, State) ->
    Reply = jsx:term_to_json([{appmon, Stats}]),
    {reply, Reply, Req, State};

websocket_handle({bigwig, _}, Req, State) ->
    {ok, Req, State};

websocket_handle(Msg, Req, State) ->
    io:format("Unhandled msg to ~p ~p\n", [?MODULE, Msg]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) -> ok.

