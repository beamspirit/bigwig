%% This is a bare bones version of sasl/rb, but modified to load reports and
%% return them from calls, instead of printing to stdout.
%%
%% One problem with rb is that there is no real permalink for reports, on each
%% rescan reports are all renumbered with the most recent at number 1.
%%
%% This version loads reports alongside the index, calculates a hash to be used
%% as a unique identifier on the web interface, and a rewritten filter system
%%
-module(bigwig_report_reader).

-behaviour(gen_server).

-export([start_link/1, start/0, start/1, stop/0, rescan/0, rescan/1]).
-export([load_number/1, load_list/0, load_list/1, make_filter/1, fmt_report/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

-record(state,  {dir, data, device, max, type, abort, log}).

-record(filter, {type=all,
                 level=all,
                 startdate="0000-00-00 00:00:00",
                 enddate="9999-99-99 99:99:99",
                 limit=50 
             }).

%%-----------------------------------------------------------------
start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup, 
			   {bigwig_report_reader_server, {bigwig_report_reader, start_link, [Options]},
			    temporary, brutal_kill, worker, [bigwig_report_reader]}).

start_link(Options) ->
    gen_server:start_link({local, bigwig_report_reader_server}, bigwig_report_reader, Options, []).

stop() -> 
    supervisor:terminate_child(sasl_sup, bigwig_report_reader_server).

rescan() -> rescan([]).
rescan(Options) ->
    call({rescan, Options}).


load_list() -> load_list(#filter{}).

load_list(F = #filter{}) -> call({load_list, F}).

%% Load a report by its number
load_number(Number) when is_integer(Number) -> 
    call({load_number, Number}).

make_filter(Props) ->
    build_filter(#filter{}, Props).

%%-----------------------------------------------------------------

call(Req) ->
    gen_server:call(bigwig_report_reader_server, Req, infinity).

build_filter(F, []) -> F;
build_filter(F, [{limit, L}|Rest]) when is_integer(L) ->
    build_filter(F#filter{limit=L}, Rest);
build_filter(F, [{type, T}|Rest]) when is_atom(T) ->
    build_filter(F#filter{type=T}, Rest);
build_filter(F, [{level, L}|Rest]) when is_atom(L) ->
    build_filter(F#filter{level=L}, Rest);
build_filter(F, [{startdate, D}|Rest]) when is_list(D) ->
    build_filter(F#filter{startdate=D}, Rest);
build_filter(F, [{enddate, D}|Rest]) when is_list(D) ->
    build_filter(F#filter{enddate=D}, Rest);
build_filter(_F, [Err]) ->
    throw({invalid_bigwig_report_reader_filter_param, Err}).

%%-----------------------------------------------------------------

init(Options) ->
    process_flag(priority, low),
    process_flag(trap_exit, true),
    Log = get_option(Options, start_log, standard_io),
    Device = open_log_file(Log),
    Dir = get_report_dir(Options),
    Max = get_option(Options, max, all),
    Type = get_option(Options, type, all),
    Abort = get_option(Options, abort_on_error, false),
    Data = scan_files(Dir ++ "/", Max, Type),
    {ok, #state{dir = Dir ++ "/", data = Data, device = Device,
		max = Max, type = Type, abort = Abort, log = Log}}.

handle_call({rescan, Options}, _From, State) ->
    {Device,Log1} = 
	case get_option(Options, start_log, {undefined}) of
	    {undefined} -> 
		{State#state.device,State#state.log};
	    Log ->
		close_device(State#state.device),
		{open_log_file(Log),Log}
	end,
    Max = get_option(Options, max, State#state.max),
    Type = get_option(Options, type, State#state.type),
    Abort = get_option(Options, abort_on_error, false),
    Data = scan_files(State#state.dir, Max, Type),
    NewState = State#state{data = Data, max = Max, type = Type,
			   device = Device, abort = Abort, log = Log1},
    {reply, ok, NewState};

handle_call(_, _From, #state{data = undefined}) ->
    {reply, {error, no_data}, #state{}};

handle_call({load_list, #filter{type=FType,
                                level=FLevel,
                                startdate=FStartDate, 
                                enddate=FEndDate,
                                limit=Limit}}, _From, State) ->
    Reports = [ {No, RealType, ShortDesc, Date} 
                || {No, RealType, ShortDesc, Date, _Fname, _Fpos} 
                <- State#state.data, 
                   (FType == all orelse RealType == FType) andalso
                   Date >= FStartDate andalso
                   Date =< FEndDate 
              ],
    
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,

    {_,Reports2} = lists:foldl( fun( {No, RealType, ShortDesc, _Date}, {Total,Acc} ) ->
                            {ok, Date2, Msg, Str} = load_report_by_num(Dir, Data, No, Device, Abort, Log),
                            case Total < Limit andalso
                                 (FLevel =:= all orelse
                                 (catch proplists:get_value(report_level, Msg)) =:= FLevel) of
                                 true ->
                                    Hash = bigwig_util:md5(term_to_binary(Msg)),
                                    {Total+1, [{Hash, RealType, ShortDesc, Date2, Msg, Str}|Acc]};
                                 false ->
                                    {Total,Acc}
                             end
                     end, {0,[]}, lists:reverse(Reports)),
    
    {reply, Reports2, State};

handle_call({load_number, Number}, _From, State) ->
    #state{dir = Dir, data = Data, device = Device, abort = Abort, log = Log} = State,
    Ret = load_report_by_num(Dir, Data, Number, Device, Abort, Log),
    {reply, Ret, State#state{device = Device}}.

terminate(_Reason, #state{device = Device}) ->
    close_device(Device).

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Func: open_log_file/1
%% Args: FileName | standard_io
%% Returns: A Device for later use in call to io:format
%%-----------------------------------------------------------------
open_log_file(standard_io) -> standard_io;
open_log_file(FileName) ->
    case file:open(FileName, [write,append]) of
	{ok, Fd} -> Fd;
	Error -> 
	    io:format("rb: Cannot open file '~s' (~w).~n",
		      [FileName, Error]),
	    io:format("rb: Using standard_io~n"),
	    standard_io
    end.

close_device(Fd) when is_pid(Fd) ->
    catch file:close(Fd);
close_device(_) -> ok.

get_option(Options, Key, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> Value;
	_ -> Default
    end.

get_report_dir(Options) ->
    case lists:keysearch(report_dir, 1, Options) of
	{value, {_Key, RptDir}} -> RptDir;
	_ ->
	    case catch application:get_env(sasl, error_logger_mf_dir) of
		{ok, Dir} -> Dir;
		_ ->
		    exit("cannot locate report directory")
	    end
    end.

%%-----------------------------------------------------------------
%% Func: scan_files(RptDir, Max, Type)
%% Args: RptDir ::= string().
%%       Max ::= integer() | all, describing how many reports
%5               to read.
%%       Type ::= atom(), describing which reports to read.
%% Purpose: Scan all report files one time, and build a list of
%%          small elements 
%% Returns: Data, where Data is a list of
%%          {Number, Type, ShortDescr, Date, Fname, FilePosition}.
%%-----------------------------------------------------------------
scan_files(RptDir, Max, Type) ->
    case file:open(RptDir ++ "/index", [raw, read]) of
	{ok, Fd} ->
	    case catch file:read(Fd, 1) of
		{ok, [LastWritten]} -> 
		    file:close(Fd),
		    Files = make_file_list(RptDir, LastWritten),
		    scan_files(RptDir, Files, Max, Type);		
		_X ->
		    file:close(Fd),
		    exit("cannot read the index file")
	    end;
	_X -> exit("cannot read the index file")
    end.

%%-----------------------------------------------------------------
%% Func: scan_files(Dir, Files, Max, Type)
%% Args: Files is a list of FileName.
%% Purpose: Scan the report files in the index variable.
%% Returns: {Number, Type, ShortDescr, Date, FileName, FilePosition}
%%-----------------------------------------------------------------
scan_files(Dir, Files, Max, Type) ->
    scan_files(Dir, 1, Files, [], Max, Type).
scan_files(_Dir, _, [], Res, _Max, _Type) -> Res;
scan_files(_Dir, _, _Files, Res, Max, _Type) when Max =< 0 -> Res;
scan_files(Dir, No, [H|T], Res, Max, Type) ->
    Data = get_report_data_from_file(Dir, No, H, Max, Type),
    Len = length(Data),
    NewMax = dec_max(Max, Len),
    NewNo = No + Len,
    NewData = Data ++ Res,
    scan_files(Dir, NewNo, T, NewData, NewMax, Type).

make_file_list(Dir, FirstFileNo) ->
    case file:list_dir(Dir) of
	{ok, FileNames} ->
	    FileNumbers = lists:zf(fun(Name) ->
					   case catch list_to_integer(Name) of
					       Int when is_integer(Int) ->
						   {true, Int};
					       _ ->
						   false
					   end
				   end,
				   FileNames),
	    shift(lists:sort(FileNumbers), FirstFileNo);
	_ -> exit({bad_directory, Dir})
    end.
					  
shift(List, First) -> 
    shift(List, First, []).

shift([H | T], H, Res) ->
    [H | Res] ++ lists:reverse(T);
shift([H | T], First, Res) ->
    shift(T, First, [H | Res]);
shift([], _, Res) ->
    Res.

dec_max(all, _) -> all;
dec_max(X,Y) -> X-Y.

get_report_data_from_file(Dir, No, FileNr, Max, Type) ->	
    Fname = integer_to_list(FileNr),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, [read]) of
	{ok, Fd} when is_pid(Fd) -> read_reports(No, Fd, Fname, Max, Type);
	_ -> [{No, unknown, "Can't open file " ++ Fname, "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: read_reports(No, Fd, Fname, Max, Type)
%% Purpose: Read reports from one report file.
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePosition}
%% Note: We have to read all reports, and then check the max-
%%       variable, because the reports are reversed on the file, and
%%       we may need the last ones.
%%-----------------------------------------------------------------
read_reports(No, Fd, Fname, Max, Type) ->
    %%io:format("rb: reading report..."),
    case catch read_reports(Fd, [], Type) of
	{ok, Res} -> 
	    file:close(Fd),
	    %%io:format("done.~n"),
	    NewRes = 
		if
		    length(Res) > Max ->
			lists:sublist(Res, 1, Max);
		    true ->
			Res
		end,
	    add_report_data(NewRes, No, Fname);
	{error, [Problem | Res]} ->
	    file:close(Fd),
	    io:format("Error: ~p~n",[Problem]),
	    io:format("Salvaged ~p entries from corrupt report file ~s...~n",
		      [length(Res),Fname]),
	    NewRes = 
		if
		    length([Problem|Res]) > Max ->
			lists:sublist([Problem|Res], 1, Max);
		    true ->
			[Problem|Res]
		end,
	    add_report_data(NewRes, No, Fname);
	Else ->
	    io:format("err ~p~n", [Else]),
	    [{No, unknown, "Can't read reports from file " ++ Fname,
		  "???", Fname, 0}]
    end.

read_reports(Fd, Res, Type) ->
    {ok, FilePos} = file:position(Fd, cur),
    case catch read_report(Fd) of
	{ok, Report} -> 
	    RealType = get_type(Report),
	    {ShortDescr, Date} = get_short_descr(Report),
	    Rep = {RealType, ShortDescr, Date, FilePos},
	    if
		Type == all->
		    read_reports(Fd, [Rep | Res], Type);
		RealType == Type ->
		    read_reports(Fd, [Rep | Res], Type);
		is_list(Type) ->
		    case lists:member(RealType, Type) of
			true ->
			    read_reports(Fd, [Rep | Res], Type);
			_ ->
			    read_reports(Fd, Res, Type)
		    end;
		true ->
		    read_reports(Fd, Res, Type)
	    end;
	{error, Error} ->
	    {error, [{unknown, Error, [], FilePos} | Res]};
	eof ->
	    {ok, Res};
	{'EXIT', Reason} ->
	    [{unknown, Reason, [], FilePos} | Res]
    end.

read_report(Fd) ->
    case io:get_chars(Fd,'',2) of
        [Hi,Lo] ->
            Size = get_int16(Hi,Lo),
            case io:get_chars(Fd,'',Size) of
                eof ->
                    {error,"Premature end of file"};
                List ->
                    Bin = list_to_binary(List),
		    Ref = make_ref(),
		    case (catch {Ref,binary_to_term(Bin)}) of
			{'EXIT',_} ->
			    {error, "Incomplete erlang term in log"};
			{Ref,Term} ->
			    {ok, Term}
		    end
	    end;
        eof ->
            eof
    end.
 
get_int16(Hi,Lo) ->
    ((Hi bsl 8) band 16#ff00) bor (Lo band 16#ff).

%%-----------------------------------------------------------------
%% Func: add_report_data(Res, No, FName)
%% Args: Res is a list of {Type, ShortDescr, Date, FilePos}
%% Purpose: Convert a list of {Type, ShortDescr, Date, FilePos} to
%%          a list of {No, Type, ShortDescr, Date, FileName, FilePos}
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePos}
%%-----------------------------------------------------------------
add_report_data(Res, No, FName) ->
    add_report_data(Res, No, FName, []).
add_report_data([{Type, ShortDescr, Date, FilePos}|T], No, FName, Res) ->
    add_report_data(T, No+1, FName,
		    [{No, Type, ShortDescr, Date, FName, FilePos}|Res]);
add_report_data([], _No, _FName, Res) -> Res.

%%-----------------------------------------------------------------
%% Update these functions with the reports that should be possible
%% to browse with rb.
%%-----------------------------------------------------------------
get_type({_Time, {error_report, _Pid, {_, crash_report, _}}}) ->
    crash_report;
get_type({_Time, {error_report, _Pid, {_, supervisor_report, _}}}) ->
    supervisor_report;
get_type({_Time, {info_report, _Pid, {_, progress, _}}}) ->
    progress;
get_type({_Time, {Type, _, _}}) -> Type;
get_type(_) -> unknown.

get_short_descr({{Date, Time}, {error_report, Pid, {_, crash_report, Rep}}}) ->
    [OwnRep | _] = Rep,
    Name = 
	case lists:keysearch(registered_name, 1, OwnRep) of
	    {value, {_Key, []}} ->
		case lists:keysearch(initial_call, 1, OwnRep) of
		    {value, {_K, {M,_F,_A}}} -> M;
		    _ -> Pid
		end;
	    {value, {_Key, N}} -> N;
	    _ -> Pid
	end,
    NameStr = lists:flatten(io_lib:format("~w", [Name])),
    {NameStr, bigwig_util:date_str(Date, Time)};
get_short_descr({{Date, Time}, {error_report, Pid, {_, supervisor_report,Rep}}}) ->
    Name =
	case lists:keysearch(supervisor, 1, Rep) of
	    {value, {_Key, N}} when is_atom(N) -> N;
	    _ -> Pid
	end,
    NameStr = lists:flatten(io_lib:format("~w", [Name])),
    {NameStr, bigwig_util:date_str(Date,Time)};
get_short_descr({{Date, Time}, {_Type, Pid, _}}) ->
    NameStr = lists:flatten(io_lib:format("~w", [Pid])),
    {NameStr, bigwig_util:date_str(Date,Time)};
get_short_descr(_) ->
    {"???", "???"}.
    

load_report_by_num(Dir, Data, Number, Device, Abort, Log) ->
    case find_report(Data, Number) of
	{Fname, FilePosition} ->
	    FileName = lists:concat([Dir, Fname]),
	    case file:open(FileName, [read]) of
		{ok, Fd} -> 
		    case load_rep(Fd, FilePosition, Device, Abort, Log) of
                {ok, Date, Msg, Str} ->
                    {ok, Date, Msg, Str};
                Err ->
                    {error, Err}
            end;
		_ -> 
		    Msg = io_lib:format("rb: can't open file ~p~n", [Fname]),
		    {{error, Msg},Device}
	    end;
	no_report ->
	    {{error, no_report},Device}
    end.

find_report([{No, _Type, _Descr, _Date, Fname, FilePosition}|_T], No) ->
    {Fname, FilePosition};
find_report([_H|T], No) -> 
    find_report(T, No);
find_report([], No) ->
    io:format("There is no report with number ~p.~n", [No]),
    no_report.

load_rep(Fd, FilePosition, _Device, _Abort, _Log) ->
    case read_rep_msg(Fd, FilePosition) of
	{Date, Msg} -> 
        ReportBin = ascii_format_report(Date, Msg),
        {ok, Date, fmt_report(Msg), ReportBin};
	_           -> {error, "rb: Cannot read from file"}
    end.

ascii_format_report(Date, Report) ->
    Fn = "/tmp/rb_format.tmp", %% TODO this is horrid
    {ok, IoDevice} = file:open(Fn, [write]),
    rb_format_supp:print(Date, Report, IoDevice),
    file:close(IoDevice),
    {ok, ReportBin} = file:read_file(Fn),
    ReportBin.
    
read_rep_msg(Fd, FilePosition) ->
    file:position(Fd, {bof, FilePosition}),
    Res = 
	case catch read_report(Fd) of
	    {ok, Report} ->
		{_ShortDescr, Date} = get_short_descr(Report),
		{Date, Report};
	    _ -> error
	end,
    file:close(Fd),
    Res.


%% report formatring stuff

fmt_report({Date, {ReportLevel, GL, {Pid, CrashType, CrashReport}}}) ->
    [   {report_level, ReportLevel},
        {group_leader, GL},
        {date, Date},
        {pid, Pid},
        {report_type, CrashType}
        | format_crashreport(CrashType, CrashReport)
    ];

fmt_report({Date, {ReportLevel, GL, GenericReport}}) ->
    [   {report_level, ReportLevel},
        {group_leader, GL},
        {date, Date},
        {generic_data, lists:flatten(io_lib:format("~p",[GenericReport]))}
    ];

fmt_report({Date, UnknownReport}) ->
    [   {generic_data, lists:flatten(io_lib:format("~p",[UnknownReport]))},
        {date, Date}
    ].
        

format_crashreport(crash_report, [OwnReport, LinkReport]) -> 
    [ {crashing_process, OwnReport},
      {neighbours, LinkReport}
    ];

format_crashreport(supervisor_report, Data) ->
    SuperName = get_opt(supervisor, Data),
    ErrorContext = get_opt(errorContext, Data),
    Reason = get_opt(reason, Data),
    ChildInfo = get_opt(offender, Data),
    [   {supervisor_name, SuperName},
        {error_context, ErrorContext},
        {reason, Reason},
        {child_info, lists:map(fun(CI) -> transform_mfa(CI) end, ChildInfo)}
    ];

format_crashreport(_, Data) -> 
    [ 
        {data, Data}
    ].

transform_mfa({mfa, Value}) -> {start_function, Value};
transform_mfa(X) -> X.

get_opt(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {_Key, Val}} -> Val;
	_  -> undefined
    end.





