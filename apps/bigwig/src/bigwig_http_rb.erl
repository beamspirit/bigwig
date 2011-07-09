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
handle_path([<<"rb">>, <<"reports">>], Req0, State) ->
    %% A version of qs_val that url-decodes values (ie no %20 etc)
    Qsval = fun(K,R) -> case cowboy_http_req:qs_val(K, R) of
                            {undefined, R2} -> {undefined, R2};
                            {ValEnc, R2}    -> {list_to_binary(bigwig_util:url_decode(ValEnc)), R2}
                        end
            end,
    %% Create a rb filter based on query params
    {Opts1,Req1} = case Qsval(<<"type">>, Req0) of
                    {undefined, R1} -> {[], R1};
                    {TypeBin, R1}   -> {[{type, list_to_atom(binary_to_list(TypeBin))}], R1}
                   end,
    {Opts2,Req2} = case Qsval(<<"startdate">>, Req1) of
                    {undefined, R2} -> {Opts1, R2};
                    {SD, R2}        -> {[{startdate, binary_to_list(SD)}|Opts1], R2}
                   end,
    {Opts3,Req3} = case Qsval(<<"enddate">>, Req2) of
                    {undefined, R3} -> {Opts2, R3};
                    {ED, R3}        -> {[{enddate, binary_to_list(ED)}|Opts2], R3}
                   end,
    {Opts4,Req4} = case Qsval(<<"limit">>, Req3) of
                    {undefined, R4} -> {Opts3, R4};
                    {IntStr, R4}    -> {[{limit, list_to_integer(binary_to_list(IntStr))}|Opts3], R4}
                   end,
    {Opts5,Req5} = case Qsval(<<"level">>, Req4) of
                    {undefined, R5} -> {[], R5};
                    {LevelBin, R5}   -> {[{level, list_to_atom(binary_to_list(LevelBin))}|Opts4], R5}
                   end,

    ReportFilter = rb2:make_filter(Opts5),
    
    Body = jsx:term_to_json(list_reports(ReportFilter)),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req6} = cowboy_http_req:reply(200, Headers, Body, Req5),
    {ok, Req6, State};

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


list_reports(Filter)  -> 
    Reports = rb2:load_list(Filter),
    format_reports(Reports).

format_reports(Reports) ->
    [ [    {hash, list_to_binary(Hash)},
           {type, list_to_binary(atom_to_list(Type))},
           {pid,  list_to_binary(Pid)},
           {date, list_to_binary(Date)},
           {report, Rep},
           {report_str, Str}
        ]
        || {Hash,Type,Pid,Date,Rep,Str} <- Reports ].


