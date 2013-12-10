%%
%% show details on a specific process
%%
-module(bigwig_http_entop).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_req:path(Req0),
    {Method, Req1} = cowboy_req:method(Req),
   
    handle_path(Method, Path, Req1, State).

handle_path(<<"GET">>, <<"/entop">>, Req, State) ->
    handle_get(Req, State);
handle_path(_, _, Req, State) ->
    not_found(Req, State).

not_found(Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_get(Req, State) ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Info = update(),
    Body = jsx:term_to_json(Info),
    {ok, Req2} = cowboy_req:reply(200, Headers,  Body, Req),
    {ok, Req2, State}.

update() ->
    PidList=erlang:processes(),
    Ps = [entop_proc_info_to_json(P) || P <- PidList],
    NumPs = length(Ps),
    [{<<"iTotalRecords">>, NumPs},
     {<<"iTotalDisplayRecords">>, NumPs},
     {<<"sColumns">>, <<"pid,name,reds,mq,hsize,ssize,htot">>},
     {<<"aaData">>, Ps}].
entop_proc_info_to_json(P) ->
    ProcInfo = erlang:process_info(P, [registered_name,
                            reductions,
                            message_queue_len,
                            heap_size,
                            stack_size,
                            total_heap_size]),
    case ProcInfo of
        undefined -> [];
        _ ->
            Name = proplists:get_value(registered_name, ProcInfo, undefined),
            Reds = proplists:get_value(reductions, ProcInfo, 0),
            Mq = proplists:get_value(message_queue_len, ProcInfo, 0),
            Hsize = proplists:get_value(heap_size, ProcInfo, 0),
            Ssize = proplists:get_value(stack_size, ProcInfo, 0),
            Htot = proplists:get_value(total_heap_size, ProcInfo, 0),
            [ list_to_binary(pid_to_list(P)), Name, Reds, Mq, Hsize, Ssize, Htot ]
    end.
