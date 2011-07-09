%%
%% show details on the VM, releases, apps, etc.
%%
-module(bigwig_http_wh).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3]).

-compile(export_all).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_http_websocket}.

handle(_Req, _State) ->
  error(websockets_only).

terminate(_Req, _State) ->
  error(websockets_only).

websocket_init(_TransportName, Req, _Opts) ->
  send([
      {title, <<"What's Happening">>},
      apps()
    ]),
  {ok, Req, undefined_state}.

websocket_handle({timeout, _Ref, Msg}, Req, State) ->
  {reply, Msg, Req, State};

websocket_handle({websocket, Msg}, Req, State) ->
  Term = jsx:json_to_term(Msg),
  {reply, jsx:term_to_json(Term), Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

send(T) when not is_binary(T) ->
  send(jsx:term_to_json(T));

send(Bin) ->
  erlang:start_timer(0, self(), Bin).

apps() ->
  {app, [
      [
        {id, App},
        {name, App},
        {description, list_to_binary(Desc)},
        {version, list_to_binary(Vsn)}
      ]
      || {App, Desc, Vsn} <- application:which_applications()
    ]}.
