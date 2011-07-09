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
    Body = mochijson2:encode(list_reports()),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
    {ok, Req2, State};

%% /rb/reports/123
handle_path([<<"rb">>, <<"reports">>, IdBin], Req, State) ->
    Id = list_to_integer(binary_to_list(IdBin)),
    Report = rb2:load_number(Id),
    %% TODO convert this to JSON:
    %%  {ok,"2011-07-09 11:42:37",
    %%      [{report_level,info_report},
    %%       {group_leader,<0.36.0>},
    %%       {date,{{2011,7,9},{11,42,37}}},
    %%       {pid,<0.39.0>},
    %%       {report_type,progress},
    %%       {data,[{supervisor,{local,sasl_sup}},
    %%              {started,[{pid,<0.170.0>},
    %%                        {name,rb2_server},
    %%                        {mfargs,{rb2,start_link,[[]]}},
    %%                        {restart_type,temporary},
    %%                        {shutdown,brutal_kill},
    %%                        {child_type,worker}]}]}]}
    %% 
    Body = io_lib:format("~p",[Report]),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
    {ok, Req2, State};

handle_path(Path, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [], io_lib:format("Not found: ~p", [Path]), Req), %% FIXME injection
    {ok, Req2, State}.


terminate(_Req, _State) ->
    ok.


list_reports() ->
    {struct,
    lists:map( fun({Id,Type,Pid,Date}) ->
                {Id, {struct, [ {uri,  list_to_binary(io_lib:format("/rb/reports/~B", [Id]))},
                                {type, list_to_binary(atom_to_list(Type))},
                                {pid,  list_to_binary(Pid)},
                                {date, list_to_binary(Date)} ]}}
               end, 
               rb2:load_list()
             )}.
