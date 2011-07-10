%%
%% report browser streaming of new SASL reports via websocket
%%
-module(bigwig_http_rb_stream).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3]).

-define(DEFAULT_COUNT, 50).

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
  bigwig_error_handler:register_client(self()),
  Self = self(),
  io:format("Self ~p~n", [Self]),
  {ok, Req, undefined_state}.

websocket_handle(Bin, Req, State) when is_binary(Bin) ->
  {reply, Bin, Req, State};

%% handle sasl reports sent form our custom handler
websocket_handle({bigwig_error_handler, Report}, Req, State) ->
  {reply, report(bigwig_report_reader:fmt_report({erlang:localtime(), Report})), Req, State};

websocket_handle({websocket, Msg}, Req, State) ->
  {reply, << "You said: ", Msg/binary >>, Req, State};

websocket_handle(Msg, Req, State) ->
  io:format("Unknown msg to ~p ~p~n", [?MODULE, Msg]),
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
          
read_history(To) ->
  lists:foreach(
    fun(Seq) ->
      number(To, bigwig_report_reader:load_number(?DEFAULT_COUNT + 1 - Seq))
    end, 
    lists:seq(0, ?DEFAULT_COUNT)).

number(To, {_Level, {ok, _Date0, Report, _ReportStr}}) ->
  report(To, Report);
number(_, _) -> ok.

report(To, Report) ->
  To ! report(Report).

report(Report) ->
  %io:format("Date0 ~p~n", [Date0]),
  %io:format("Report ~p~n", [Report]),
  %io:format("ReportStr ~p~n", [ReportStr]),
  jsx:term_to_json([{report, [Report]}]).
