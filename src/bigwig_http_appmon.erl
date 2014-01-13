%%
%% web service supporting appmon
%%
-module(bigwig_http_appmon).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-define(P2B(P), list_to_binary(pid_to_list(P))).
-define(L2B(L), list_to_binary(L)).
-define(B2EA(B), (catch(list_to_existing_atom(binary_to_list(B))))).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_req:path_info(Req0),
    {Method, Req1} = cowboy_req:method(Req),
    handle_path(Method, Path, Req1, State).

handle_path(<<"GET">>, [<<"_all">>], Req, State) ->
    {ok, RawApps} = bigwig_appmon_info:node_apps(),
    Info =
        [{?P2B(Pid),
          [{name, App},
           {desc, ?L2B(Desc)},
           {ver, ?L2B(Ver)}]} || {Pid, _, {App, Desc, Ver}} <- RawApps],
    Body = jsx:term_to_json(Info),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State};
handle_path(<<"GET">>, [App0], Req, State) ->
    case ?B2EA(App0) of
        App when is_atom(App) ->
            {ok, Info} = bigwig_appmon_info:calc_app_tree(App),
            Body = jsx:term_to_json(Info),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
            {ok, Req2, State};
        _ ->
            not_found(Req, State)
    end;
handle_path(_, _, Req, State) ->
    not_found(Req, State).

not_found(Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
