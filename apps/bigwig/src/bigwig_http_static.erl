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
  {[_|Path], _} = cowboy_http_req:path(Req), % strip <<"static">>
  send(Req, Path, State);

handle(Req, OnlyFile = State) ->
  send(Req, OnlyFile, State).

send(Req, Path, State) ->
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
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
  file:read_file(check(Priv, filename:join(Priv, Path))).

priv() ->
  code:priv_dir(bigwig).

check(Priv, Path) when is_list(Priv) orelse is_list(Path) ->
  check(iolist_to_binary(Priv), iolist_to_binary(Path));
check(Priv, Path) ->
  Size = byte_size(Priv),
  <<CheckPriv:Size/binary-unit:8, _/binary>> = Path,
  case CheckPriv of
    Priv -> Path;
    _ -> throw({unauthorized, Path})
  end.
