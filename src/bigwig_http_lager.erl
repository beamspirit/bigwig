%%
%% show details on a specific process
%%
-module(bigwig_http_lager).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_req:path(Req0),
    {Method, Req1} = cowboy_req:method(Req),
    io:format("Path is ~p,Method is ~p",[Path,Method]),
    Path1=lists:delete(<<>>,binary:split(Path,[<<"/">>],[global])),
    handle_path(Method, Path1, Req1, State).

handle_path(<<"GET">>, [<<"lager">>, <<"status">>], Req, State) ->
    handle_get_status(Req, State);
handle_path(<<"GET">>, [<<"lager">>, <<"tracer">>, RoutingKey], Req, State) ->
    handle_get_log(RoutingKey, Req, State);
handle_path(<<"PUT">>, [<<"lager">>, <<"tracer">>, Tracer], Req, State) ->
    handle_add_tracer(Tracer, Req, State);
handle_path(<<"DELETE">>, [<<"lager">>, <<"tracer">>, <<"all">>], Req, State) ->
    handle_clear_traces(Req, State);
handle_path(<<"DELETE">>, [<<"lager">>, <<"tracer">>, Tracer], Req, State) ->
    handle_del_trace(Tracer, Req, State);
handle_path(_, _, Req, State) ->
    not_found(Req, State).

handle_get_status(Req,State) ->
  Status = lager:status(),
  Status1 = remove(Status),
  Body = jsx:term_to_json(Status1),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.
handle_get_log(_RoutingKey, Req, State) ->
  {ok, Req, State}.
handle_add_tracer(Tracer, Req, State) ->

  io:format("tracer is ~p", [Tracer]),
  All = binary:split(Tracer,[<<",">>],[global]),
  
  amqp_tracer:start_link(),
    case length(All) of
      5 ->
            Attr1 = binary_to_atom(lists:nth(1, All), utf8),
            RoutingKey = lists:nth(2, All),
            Filter = [{binary_to_atom(lists:nth(3, All), utf8), list_to_integer(binary_to_list(lists:nth(4, All)))}],
            Level = binary_to_atom(lists:nth(5, All), utf8),
            amqp_tracer:trace_amqp(Attr1, RoutingKey, Filter, Level);
      3 ->
            RoutingKey = lists:nth(1, All),
            Filter = [{binary_to_atom(lists:nth(2, All), utf8), list_to_integer(binary_to_list(lists:nth(3, All)))}],
            amqp_tracer:trace_amqp(RoutingKey, Filter);
      4 ->
            RoutingKey = lists:nth(1, All),
            Filter = [{binary_to_atom(lists:nth(2, All), utf8), list_to_integer(binary_to_list(lists:nth(3, All)))}],
            Level = binary_to_atom(lists:nth(4, All), utf8),
            amqp_tracer:trace_amqp(RoutingKey, Filter, Level);
      _ -> ok
    end,
    {ok,Req,State}.
handle_del_trace(Tracer, Req, State) ->
  io:format("tracer is ~p", [Tracer]),
  All = binary:split(Tracer,[<<",">>],[global]),
  
  amqp_tracer:start_link(),
    case length(All) of
      5 ->
            Attr1 = binary_to_atom(lists:nth(1, All), utf8),
            RoutingKey = lists:nth(2, All),
            Filter = [{binary_to_atom(lists:nth(3, All), utf8), list_to_integer(binary_to_list(lists:nth(4, All)))}],
            Level = binary_to_atom(lists:nth(5, All), utf8),
            Trace0 = { Filter, Level, {lager_amqp_backend, RoutingKey} },
            case lager_util:validate_trace(Trace0) of
              {ok, Trace} ->
                   amqp_tracer:stop_trace(Attr1, Trace);
              Error ->
                  Error
            end;
      4 ->
            RoutingKey = lists:nth(1, All),
            Filter = [{binary_to_atom(lists:nth(2, All), utf8), list_to_integer(binary_to_list(lists:nth(3, All)))}],
            Level = binary_to_atom(lists:nth(4, All), utf8),
            Trace0 = { Filter, Level, {lager_amqp_backend, RoutingKey} },
            case lager_util:validate_trace(Trace0) of
              {ok, Trace} ->
                  amqp_tracer:stop_trace(Trace);
              Error ->
                  Error
            end;
      _ -> ok
    end,
    {ok,Req,State}.  
handle_clear_traces(Req, State) ->
  lager:clear_all_traces(),
  {ok,Req,State}.
not_found(Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

remove(Status) ->
   [_,LStatus,_,Traces,Reductions,_] = Status,
   LStatus1=lists:delete([],LStatus),
   [_, Reductions1] = Reductions,
   Reductions2 = lists:delete("\n",Reductions1),
   Status1 = [
               {<<"Lager status">>, LStatus1}, 
               {<<"ActiveTraces">>, Traces}, 
               {<<"Tracing Reductions">>,Reductions2}
               ],
   Status1.