%%
%% report browser api
%%
-module(bigwig_http_rb).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    rb2:start(), %% will only be started once anyway, registered name
    rb2:rescan(), %% ouch
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Path, Req2} = cowboy_http_req:path(Req),
    handle_path(Path, Req2, State).

%% /rb/reports
handle_path([<<"rb">>, <<"reports">>], Req, State) ->
    Body = jsx:term_to_json(list_reports()),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
    {ok, Req2, State};

%% /rb/reports/123
handle_path([<<"rb">>, <<"reports">>, IdBin], Req, State) ->
    Id = list_to_integer(binary_to_list(IdBin)),
    Rep = rb2:load_number(Id),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_http_req:reply(200, Headers, report_to_json(Rep), Req),
    {ok, Req2, State};

handle_path(Path, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [], io_lib:format("Not found: ~p", [Path]), Req), %% FIXME injection
    {ok, Req2, State}.


terminate(_Req, _State) ->
    ok.


report_to_json({_, {ok, Date0, Report, ReportStr}}) ->
    Date = list_to_binary(Date0),
    jsx:term_to_json([{date, Date}, {report, Report}, {report_str, ReportStr}]).


list_reports() ->
    lists:map( fun({Id,Type,Pid,Date}) ->
                {list_to_binary(integer_to_list(Id)),
                 [ {uri,  list_to_binary(io_lib:format("/rb/reports/~B", [Id]))},
                   {type, list_to_binary(atom_to_list(Type))},
                   {pid,  list_to_binary(Pid)},
                   {date, list_to_binary(Date)} ]}
               end, 
               rb2:load_list()
              ).
