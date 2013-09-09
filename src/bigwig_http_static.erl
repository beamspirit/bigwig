%%
%% show details on the VM, releases, apps, etc.
%%
-module(bigwig_http_static).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-export([html/1, css/1, js/1]).

-compile(export_all).

init({tcp, http}, Req, []) ->
  {ok, Req, undefined_state};
init({tcp, http}, Req, OnlyFile) ->
  {ok, Req, OnlyFile}.

handle(Req, undefined_state = State) ->
  {Path, Req2} = cowboy_req:path_info(Req), % strip <<"static">>
  send(Req2, Path, State);

handle(Req, OnlyFile = State) ->
  send(Req, OnlyFile, State).

send(Req, PathBins, State) ->
  Path = [ binary_to_list(P) || P <- PathBins ],
  case file(filename:join(Path)) of
    {ok, Body} ->
      Headers = [{<<"Content-Type">>, <<"text/html">>}],
      {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
      {ok, Req2, State};
    _ ->
      {ok, Req2} = cowboy_req:reply(404, [], <<"404'd">>, Req),
      {ok, Req2, State}
  end.

terminate(_Reason, _Req, _State) ->
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
