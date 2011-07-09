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
  send({struct, [
        {title, <<"So Peter... What's Happening?">>},
        apps()
      ]}),
  {ok, Req, undefined_state}.

websocket_handle({timeout, _Ref, Msg}, Req, State) ->
  {reply, Msg, Req, State};

websocket_handle({websocket, Msg}, Req, State) ->
  {reply, <<"Hello ", Msg/binary>>, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

send(T) when is_tuple(T) ->
  send(iolist_to_binary(mochijson2:encode(T)));

send(Bin) ->
  erlang:start_timer(0, self(), Bin).

apps() ->
  {apps, [
      {struct, [{id, App}]}
      || {App, _Desc, _Vsn} <- application:which_applications()
    ]}.
