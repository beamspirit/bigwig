%%
%% show details on a specific module
%%
-module(bigwig_http_module).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_req:path_info(Req0),
    {Method, Req1} = cowboy_req:method(Req),
    handle_path(Method, Path, Req1, State).

handle_path(<<"POST">>, [Module], Req, State) ->
    {Props, Req2} = cowboy_req:body_qs(Req),
    case proplists:get_value(<<"reload">>, Props) of
        undefined -> 
            not_found(Req2, State);
        <<"yes">> -> 
            c:l(list_to_existing_atom(binary_to_list(Module))),
            {ok, Req3} = cowboy_req:reply(200, [], <<"ok">>, Req2),
            {ok, Req3, State}
    end;
handle_path(<<"GET">>, [Module], Req, State) ->
    case to_module_info(Module) of
        [_|_] = Info -> json_response(Info, Req, State);
        _ -> not_found(Req, State)
    end;
handle_path(_, _, Req, State) ->
    not_found(Req, State).

not_found(Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% Reads and modifies module_info a bit to be more json friendly
-spec to_module_info(binary()) -> list().
to_module_info(Bin) ->
    ModInfo =  case catch(list_to_existing_atom(binary_to_list(Bin))) of
        Mod when is_atom(Mod) -> catch(Mod:module_info());
        _ -> []
    end,
    Exports = proplists:get_value(exports, ModInfo),
    NewExports = [ list_to_binary(lists:flatten(io_lib:format("~w/~w",[F,A]))) || {F,A} <- Exports ],
    ModInfo2 = [ {exports, NewExports} | proplists:delete(exports, ModInfo) ],
    ModInfo3 = 
        case proplists:get_value(compile, ModInfo2) of
            L when is_list(L) ->
                case proplists:get_value(time, L) of
                    T when is_tuple(T) ->
                        proplists:delete(compile, L) ++
                        [ {compile, [ {time, [{'_type',<<"date">>},{data,tuple_to_list(T)}]}]} 
                                    | proplists:delete(time, L) ];
                    _ ->
                        ModInfo2
                end;
            _ -> 
                ModInfo2
        end,
    ModInfo3.



json_response(Info, Req, State) ->
    Body = jsx:term_to_json(Info),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.
