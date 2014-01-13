%%
%% show details on a specific process
%%
-module(bigwig_http_etop2).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_req:path_info(Req0),
    {Method, Req1} = cowboy_req:method(Req),
    handle_path(Method, Path, Req1, State).

handle_path(<<"GET">>, [], Req, State) ->
    handle_get(Req, State);
handle_path(<<"POST">>, [<<"config">>, Key, Value], Req, State) ->
    handle_post_config(Key, Value, Req, State);
handle_path(<<"POST">>, [<<"node">>, Node], Req, State) ->
    handle_post_node(fun to_node/1, Node, Req, State);
handle_path(_, _, Req, State) ->
    not_found(Req, State).

not_found(Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_get(Req, State) ->
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  Info = etop2:update(),
  Body = jsx:term_to_json(Info),
  {ok, Req2} = cowboy_req:reply(200, Headers,  Body, Req),
  {ok, Req2, State}.

handle_post_config(Key, Value, Req, State) ->
  post_config_response(Key, Value, Req, State).

handle_post_node(Get, Node, Req, State) ->
  case catch(Get(Node)) of
    A when is_atom(A) -> post_node_response(A, Req, State);
    _                 -> not_found(Req, State)
  end.

post_config_response(Key, Value, Req, State) ->
  {ok, Req1} =
    case etop2:config(Key, Value) of
      ok    -> cowboy_req:reply(204, [], <<>>, Req);
      Error -> cowboy_req:reply(403, [], Error, Req)
    end,
    {ok, Req1, State}.

post_node_response(Node, Req, State) ->
  {ok, Req1} =
    case etop2:node(Node) of
      ok    -> cowboy_req:reply(204, [], <<>>, Req);
      Error -> cowboy_req:reply(404, [], Error, Req)
    end,
    {ok, Req1, State}.

-spec to_node(binary()) -> node().
to_node(Name) ->
    list_to_atom(binary_to_list(Name)).
