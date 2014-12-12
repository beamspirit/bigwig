%%
%% show details on a specific module
%%
-module(bigwig_http_module).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_req:path(Req0),
    {Method, Req1} = cowboy_req:method(Req),
    Path1=lists:delete(<<>>,binary:split(Path,[<<"/">>],[global])),
    handle_path(Method, Path1, Req1, State).

handle_path(<<"POST">>, [<<"module">>, Module], Req, State) ->
    {Props, Req2} = cowboy_req:body_qs(Req),
    case proplists:get_value(<<"reload">>, Props) of
        undefined -> 
            not_found(Req2, State);
        <<"yes">> -> 
            c:l(list_to_existing_atom(binary_to_list(Module))),
            {ok, Req3} = cowboy_req:reply(200, [], <<"ok">>, Req2),
            {ok, Req3, State}
    end;
handle_path(<<"GET">>, [<<"module">>, Module], Req, State) ->
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
                       {Year,Month,Day,Hour,Minute,Sec}=T,
                        proplists:delete(compile, L) ++
                        [ {compile, [ {time, [{'_type',<<"date">>},{data,[list_to_binary(integer_to_list(Year))
                            ,list_to_binary(integer_to_list(Month)),list_to_binary(integer_to_list(Day)),list_to_binary(integer_to_list(Hour)),
                            list_to_binary(integer_to_list(Minute)),list_to_binary(integer_to_list(Sec))]}]}]} 
                                    | proplists:delete(time, L) ];
                    _ ->
                        ModInfo2
                end;
            _ -> 
                ModInfo2
        end,
    ModInfo4 = 
        case proplists:get_value(time, ModInfo3) of
              T1 when is_tuple(T1) ->
                  {Year1,Month1,Day1,Hour1,Minute1,Sec1}=T1,
                  Time = list_to_binary([integer_to_list(Year1)
                            ,integer_to_list(Month1),integer_to_list(Day1),integer_to_list(Hour1),
                            integer_to_list(Minute1),integer_to_list(Sec1)]),
                  [ {time, Time} | proplists:delete(time,ModInfo3)];
              _ ->
                  ModInfo3
        end,
    ModInfo5 = 
        case proplists:get_value(source, ModInfo4) of
            L1 when is_list(L1) ->
              [ {source, list_to_binary(L1)} | proplists:delete(source,ModInfo4)];
            _ -> ModInfo4
        end,
    ModInfo6 = 
        case proplists:get_value(version, ModInfo5) of
            L2 when is_list(L2) ->
              [ {version, list_to_binary(L2)} | proplists:delete(version,ModInfo5)];
            _ -> ModInfo5
        end,    
    ModInfo7=lists:ukeysort(1,ModInfo6),
    ModInfo8= proplists:delete(options, ModInfo7),
    
    ModInfo8.



json_response(Info, Req, State) ->
    Body = jsx:term_to_json(Info),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.
