%% Websocket connection that sends node_sub_count and node_sub_detail of market_dispatcher node
%%
-module(bigwig_http_md_stream).
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
    bigwig_pubsubhub:register_client(self()),
    {ok, Req, undefined_state}.

%% TODO handle stuff like {bigwig, {appmon, ... }} and send that too

websocket_info({bigwig, {market_dispatcher, NodeClientInstrumentCount}}, Req, State) ->
    Reply = jsx:term_to_json(NodeClientInstrumentCount),
    {reply, {text, Reply}, Req, State};


websocket_info({bigwig, _}, Req, State) ->
    {ok, Req, State};

websocket_info(Info, Req, State) ->
    lager:debug("Unhandled msg to ~p ~p\n", [?MODULE, Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) -> ok.

websocket_handle(_Msg, Req, State) ->
    {ok, Req, State}.

