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
  bigwig_report_reader:start(),  %% will only be started once anyway, registered name
  bigwig_report_reader:rescan(), %% ouch
  bigwig_pubsubhub:register_client(self()),
  {ok, Req, undefined_state}.

websocket_handle(Bin, Req, State) when is_binary(Bin) ->
  {reply, Bin, Req, State};

%% handle sasl reports sent form our custom handler
websocket_handle({bigwig, {bigwig_error_handler, Report}}, Req, State) -> 
  {reply, report(bigwig_report_reader:fmt_report({erlang:localtime(), Report})), Req, State};

%% ignore other bigwig internal msgs
websocket_handle({bigwig, _}, Req, State) ->
  {ok, Req, State};

websocket_handle({websocket, Msg}, Req, State) ->
  {reply, << "You said: ", Msg/binary >>, Req, State};

websocket_handle(Msg, Req, State) ->
  io:format("Unknown msg to ~p ~p~n", [?MODULE, Msg]),
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

report(Report) ->
  Str = bigwig_report_reader:ascii_format_report("",Report),
  J = [{report, [ {'_str', Str}|Report ]}],
  Term = jsx:term_to_json(J),
  io:format("~p\n", [Term]),
  Term.
