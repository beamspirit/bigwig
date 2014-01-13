%%
%% report browser api
%%
-module(bigwig_http_rb).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    bigwig_report_reader:start(), %% will only be started once anyway, registered name
    bigwig_report_reader:rescan(), %% ouch
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Path, Req2} = cowboy_req:path_info(Req),
    handle_path(Path, Req2, State).

%% /rb/reports
handle_path([<<"rb">>, <<"reports">>], Req0, State) ->
    {ReportFilter, Req} = make_report_filter_from_qs(Req0),
    Body = jsx:term_to_json(list_reports(ReportFilter)),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State};

%% /rb/reports/123
handle_path([<<"rb">>, <<"reports">>, IdBin], Req, State) ->
    Id = list_to_integer(binary_to_list(IdBin)),
    Rep = bigwig_report_reader:load_number(Id),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, report_to_json(Rep), Req),
    {ok, Req2, State};

handle_path(Path, Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], io_lib:format("Not found: ~p", [Path]), Req), %% FIXME injection
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.


report_to_json({_, {ok, Date0, Report, ReportStr}}) ->
    Date = list_to_binary(Date0),
    jsx:term_to_json([{date, Date}, {report, Report}, {report_str, ReportStr}]).


list_reports(Filter)  -> 
    Reports = bigwig_report_reader:load_list(Filter),
    format_reports(Reports).

format_reports(Reports) -> 
    [{report, lists:map(fun format_report/1, Reports)}].

%% NB: added a is_list guard, hpefully all reports are proplists here?
%% if not, add a format_report that wraps it into an obj that includes hash?
format_report({Hash,_Type,_Pid,_Date,Rep,Str}) when is_list(Rep) ->
    [ {'_hash', list_to_binary(Hash)},
      {'_str',  Str}
      | Rep 
    ].

%% Make a proplist to pass to make_filter, from the querstring
make_report_filter_from_qs(Req0) ->
    %% A version of qs_val that url-decodes values (ie no %20 etc), and to_list
    Qsval = fun(K,R) -> case cowboy_req:qs_val(K, R) of
                            {undefined, R2} -> {undefined, R2};
                            {ValEnc, R2}    -> {bigwig_util:url_decode(ValEnc), R2}
                        end
            end,
    %% Create a rb filter based on query params
    {Opts1,Req1} = case Qsval(<<"type">>, Req0) of
                    {undefined, R1} -> {[], R1};
                    {TypeBin, R1}   -> {[{type, list_to_atom(TypeBin)}], R1}
                   end,
    {Opts2,Req2} = case Qsval(<<"startdate">>, Req1) of
                    {undefined, R2} -> {Opts1, R2};
                    {SD, R2}        -> {[{startdate, SD}|Opts1], R2}
                   end,
    {Opts3,Req3} = case Qsval(<<"enddate">>, Req2) of
                    {undefined, R3} -> {Opts2, R3};
                    {ED, R3}        -> {[{enddate, ED}|Opts2], R3}
                   end,
    {Opts4,Req4} = case Qsval(<<"limit">>, Req3) of
                    {undefined, R4} -> {Opts3, R4};
                    {IntStr, R4}    -> {[{limit, list_to_integer(IntStr)}|Opts3], R4}
                   end,
    {Opts5,Req5} = case Qsval(<<"level">>, Req4) of
                    {undefined, R5} -> {Opts4, R5};
                    {LevelBin, R5}   -> {[{level, list_to_atom(LevelBin)}|Opts4], R5}
                   end,
    Filter = bigwig_report_reader:make_filter(Opts5),
    {Filter, Req5}.
