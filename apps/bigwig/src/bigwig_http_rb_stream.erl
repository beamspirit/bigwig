%%
%% report browser streaming of new SASL reports via websocket
%%
-module(bigwig_http_rb_stream).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [], <<"Websockets only here pls">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.


websocket_init(_TransportName, Req, _Opts) ->
    bigwig_error_handler:register_client(self()),
    {ok, Req, undefined_state}.

websocket_handle({bigwig_error_handler, Report}, Req, State) ->
    Msg = jsx:term_to_json([{report, Report}]),
    {reply, Msg, Req, State};

websocket_handle({websocket, Msg}, Req, State) ->
    {reply, << "You said: ", Msg/binary >>, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
          
