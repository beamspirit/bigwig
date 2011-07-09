%%
%% show details on the VM, releases, apps, etc.
%%
-module(bigwig_http_wh).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-compile(export_all).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  % todo: read file into state on init
  {ok, Body} = tpl("index"),
  {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.

terminate(_Req, _State) ->
  ok.

tpl(Name) ->
  file:read_file(tpl_path(Name)).

tpl_path(Name) ->
  filename:join([code:priv_dir(bigwig), "html", Name ++ ".html"]).
