%%
%% show details on a specific module
%%
-module(bigwig_http_module).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_http_req:path(Req0),
    {Method, Req1} = cowboy_http_req:method(Req),
    handle_path(Method, Path, Req1, State).

handle_path('GET', [<<"module">>, Module], Req, State) ->
    case to_module_info(Module) of
        [_|_] = Info -> json_response(Info, Req, State);
        _ -> not_found(Req, State)
    end;
handle_path(_, _, Req, State) ->
    not_found(Req, State).

not_found(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [], <<"<h1>404</h1>">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

-spec to_module_info(binary()) -> list().
to_module_info(Bin) ->
    case catch(list_to_existing_atom(binary_to_list(Bin))) of
        Mod when is_atom(Mod) -> catch(Mod:module_info());
        _ -> []
    end.

json_response(Info, Req, State) ->
    Body = jsx:term_to_json(Info),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.
