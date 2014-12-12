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
    bigwig_pubsubhub:register_client(self()),
    {ok, Req, undefined_state}.

%% TODO handle stuff like {bigwig, {appmon, ... }} and send that too
websocket_handle({text, Msg}, Req, State) when Msg =:= <<"unsubscribe">> ->
     io:format("msg is ~p",[Msg]),
     amqp_subscriber:cast(unsubscribe),
     {ok, Req, State};
websocket_handle({text, Msg}, Req, State) when Msg =/= <<"unsubscribe">> ->
     io:format("msg is ~p",[Msg]),
     case amqp_subscriber:start_link(Msg) of
       {ok,Pid} ->
          io:format("amqp_subscriber init is ~p~n",[Pid]);
       {error,_} ->
         io:format("amqp_subscriber already start,subscribe ~n"),
         amqp_subscriber:cast(subscribe)
     end,
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

