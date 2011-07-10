%%
%% show details on the VM, releases, apps, etc.
%%
-module(bigwig_http_static).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-export([html/1, css/1, js/1]).

-compile(export_all).

init({tcp, http}, Req, []) ->
  {ok, Req, undefined_state};
init({tcp, http}, Req, OnlyFile) ->
  {ok, Req, OnlyFile}.

handle(Req, undefined_state = State) ->
  {[_|Path], Req2} = cowboy_http_req:path(Req), % strip <<"static">>
  send(Req2, Path, State);

handle(Req, OnlyFile = State) ->
  send(Req, OnlyFile, State).

send(Req, PathBins, State) ->
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  Path = [ binary_to_list(P) || P <- PathBins ],
  {ok, Body} = file(filename:join(Path)),
  {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.

terminate(_Req, _State) ->
  ok.

html(Name) ->
  type("html", Name).
css(Name) ->
  type("css", Name).
js(Name) ->
  type("js", Name).

type(Type, Name) ->
  file(filename:join([Type, Name ++ Type])).

file(Path) ->
  Priv = priv(),
  file:read_file(filename:join(Priv, Path)).

priv() ->
  case code:priv_dir(bigwig) of
    {error,_} -> "priv";
    Priv -> Priv
  end.
