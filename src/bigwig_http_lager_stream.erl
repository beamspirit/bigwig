%% Websocket connection that sends log of trace
%%
-module(bigwig_http_lager_stream).
-behaviour(cowboy_websocket_handler).

-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3,
				 websocket_info/3, websocket_terminate/3]).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

handle(_Req, _State) ->
    exit(websockets_only).

terminate(_Reason, _Req, _State) ->
    exit(websockets_only).

websocket_init(_TransportName, Req, _Opts) ->
    io:format("lager_stream init ok~n"),
    bigwig_pubsubhub:register_client(self()),
    {ok, Req, undefined_state}.

%% TODO handle stuff like {bigwig, {appmon, ... }} and send that too
websocket_handle({text, Msg}, Req, State) ->
    io:format("Msg is ~p",[Msg]),
    amqp_subscriber:start_link(Msg),
    {ok, Req, State}.
websocket_info({bigwig, {bigwig_trace, Stats}}, Req, State) ->
     Reply = jsx:term_to_json([{lager, Stats}]),
     {reply, {text, Reply}, Req, State};
websocket_info({bigwig, _}, Req, State) ->
     {ok, Req, State};
websocket_info(Info, Req, State) ->
     io:format("Unhandled msg to ~p ~p\n", [?MODULE, Info]),
     {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) -> ok.

