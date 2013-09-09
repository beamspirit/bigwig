%%
%% show details on a specific process
%%
-module(bigwig_http_pid).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_req:path_info(Req0),
    {Method, Req1} = cowboy_req:method(Req),
    handle_path(Method, Path, Req1, State).

handle_path(<<"GET">>, [<<"global">>, Name], Req, State) ->
    handle_get_pid(fun to_global_pid/1, Name, Req, State);
handle_path(<<"GET">>, [Pid], Req, State) ->
    handle_get_pid(fun to_pid/1, Pid, Req, State);
handle_path(<<"POST">>, [<<"global">>, Name], Req, State) ->
    handle_post_pid(fun to_global_pid/1, Name, Req, State);
handle_path(<<"POST">>, [Pid], Req, State) ->
    handle_post_pid(fun to_pid/1, Pid, Req, State);
handle_path(<<"DELETE">>, [Pid], Req, State) ->
    handle_delete_pid(fun to_pid/1, Pid, Req, State);
handle_path(_, _, Req, State) ->
    not_found(Req, State).

not_found(Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_get_pid(Get, Pid0, Req, State) ->
    case catch(Get(Pid0)) of
        Pid when is_pid(Pid) -> pid_response(Pid, Req, State);
        _ -> not_found(Req, State)
    end.

handle_post_pid(Get, Pid0, Req, State) ->
    case catch(Get(Pid0)) of
        Pid when is_pid(Pid) ->
            post_pid_response(Pid, Req, State);
        _ -> not_found(Req, State)
    end.

handle_delete_pid(Get, Pid0, Req, State) ->
    case catch(Get(Pid0)) of
        Pid when is_pid(Pid) ->
            erlang:exit(Pid, kill),
            {ok, Req2} = cowboy_req:reply(200, [], "killed", Req),
            {ok, Req2, State};
        _ -> not_found(Req, State)
    end.

post_pid_response(Pid, Req, State) ->
    {Res, Req1} = cowboy_req:body_qs(Req),
    Headers = [{<<"Content-Type">>, <<"application/x-erlang-term">>}],
    {ok, Req2} =
        case lists:keyfind(<<"msg">>, 1, Res) of
            {<<"msg">>, TermStr} ->
                case catch(bigwig_util:parse_term(TermStr)) of
                    {ok, Term} ->
                        Body = io_lib:format("~p", [Pid ! Term]),
                        cowboy_req:reply(202, Headers, Body, Req1);
                    _ ->
                        cowboy_req:reply(400, Headers, <<"{error, badarg}">>, Req1)
                end;
            _ ->
                cowboy_req:reply(400, Headers, <<"{error, msg_required}">>, Req1)
        end,
    {ok, Req2, State}.

-spec to_pid(binary()) -> pid() | undefined.
to_pid(Bin) when is_binary(Bin) ->
    L = binary_to_list(Bin),
    try list_to_pid([$<] ++ L ++ [$>])
    catch error:badarg -> whereis(list_to_existing_atom(L))
    end.

-spec to_global_pid(binary()) -> pid() | undefined.
to_global_pid(Name) ->
    global:whereis_name(list_to_existing_atom(binary_to_list(Name))).

pid_response(Pid, Req, State) ->
    case erlang:process_info(Pid) of
        undefined -> not_found(Req, State);
        Info ->
            Body = jsx:term_to_json(Info),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
            {ok, Req2, State}
    end.
