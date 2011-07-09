%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(etop2).
-author('siri@erix.ericsson.se').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, update/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([config/2, help/0]).
%% Internal
-export([update/1]).
-export([loadinfo/1, meminfo/2, getopt/2]).

-include("etop.hrl").
-include("etop_defs.hrl").

-define(change_at_runtime_config,[lines,interval,sort,accumulate]).

-define(SERVER, etop2_server).

help() ->
    io:format(
      "Usage of the erlang top program~n"
      "Options are set as command line parameters as in -node a@host -..~n"
      "or as parameter to etop:start([{node, a@host}, {...}])~n"
      "Options are:~n"
      "  node        atom       Required   The erlang node to measure ~n"
      "  port        integer    The used port, NOTE: due to a bug this program~n"
      "                         will hang if the port is not avaiable~n"
      "  accumulate  boolean    If true execution time is accumulated ~n"
      "  lines       integer    Number of displayed processes~n"
      "  interval    integer    Display update interval in secs~n"
      "  sort        runtime | reductions | memory | msg_q~n"
      "                         What information to sort by~n"
      "                         Default: runtime (reductions if tracing=off)~n"
      "  output      graphical | text~n"
      "                         How to present results~n"
      "                         Default: graphical~n"
      "  tracing     on | off   etop uses the erlang trace facility, and thus~n"
      "                         no other tracing is possible on the node while~n"
      "                         etop is running, unless this option is set to~n"
      "                         'off'. Also helpful if the etop tracing causes~n"
      "                         too high load on the measured node.~n"
      "                         With tracing off, runtime is not measured!~n"
      "  setcookie   string     Only applicable on operating system command~n"
      "                         line. Set cookie for the etop node, must be~n"
      "                         same as the cookie for the measured node.~n"
      "                         This is not an etop parameter~n"
     ).

terminate(_Reason, State) ->
  %% TODO: port over tracer?
  if State#opts.tracing == on -> etop_tr:stop_tracer(State);
     true -> ok
  end.

config(Key,Value) ->
    case check_runtime_config(Key,Value) of
	ok ->
	    ?SERVER ! {config,{Key,Value}},
	    ok;
	error ->
	    {error,illegal_opt}
    end.
check_runtime_config(lines,L) when is_integer(L),L>0 -> ok;
check_runtime_config(interval,I) when is_integer(I),I>0 -> ok;
check_runtime_config(sort,S) when S=:=runtime;
				  S=:=reductions;
				  S=:=memory;
				  S=:=msg_q -> ok;
check_runtime_config(accumulate,A) when A=:=true; A=:=false -> ok;
check_runtime_config(_Key,_Value) -> error.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Opts) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

handle_cast(_Msg, State) ->
    {stop, unhandled_cast, State}.
handle_info(_Info, State) ->
    {stop, unhandled_info, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

init(Opts) ->
  Config1 = handle_args(init:get_arguments() ++ Opts, #opts{}),
  Config2 = Config1#opts{server=self()},

  %% Connect to the node we want to look at
  Node = getopt(node, Config2),
  case net_adm:ping(Node) of
    pang when Node /= node() ->
      io:format("Error Couldn't connect to node ~p ~n~n", [Node]),
      help(),
      exit("connection error");
    _pong ->
      check_runtime_tools_vsn(Node)
  end,

  %% Maybe set up the tracing
  Config3 =
    if Config2#opts.tracing == on, Node /= node() ->
        %% Cannot trace on current node since the tracer will
        %% trace itself
        etop_tr:setup_tracer(Config2);
       true ->
        if Config2#opts.sort == runtime ->
            Config2#opts{sort=reductions,tracing=off};
           true ->
            Config2#opts{tracing=off}
        end
    end,
  AccumTab = ets:new(accum_tab,
                     [set,public,{keypos,#etop_proc_info.pid}]),
  Config4 = Config3#opts{accum_tab=AccumTab},

  {ok, Config4}.

check_runtime_tools_vsn(Node) ->
  case rpc:call(Node,observer_backend,vsn,[]) of
    {ok,Vsn} -> check_vsn(Vsn);
    _ -> exit("Faulty version of runtime_tools on remote node")
  end.
check_vsn(_Vsn) -> ok.

%% Handle the incoming data

update() ->
  gen_server:call(?SERVER, update).

handle_call(update, _From, State) ->
  Info = update(State),
  Json = update_json(Info, State),
  {reply, Json, State};
handle_call({config, Key, Value}, _From, State) ->
  State2 = putopt(Key, Value, State),
  {reply, ok, State2}.

update(#opts{store=Store,node=Node,tracing=Tracing}=Opts) ->
  Pid = spawn_link(Node,observer_backend,etop_collect,[self()]),
  Info = receive {Pid,I} -> I
         after 1000 -> exit(connection_lost)
         end,
  #etop_info{procinfo=ProcInfo} = Info,
  ProcInfo1 =
    if Tracing == on ->
        PI=lists:map(fun(PI=#etop_proc_info{pid=P}) ->
                         case ets:lookup(Store,P) of
                           [{P,T}] -> PI#etop_proc_info{runtime=T};
                           [] -> PI
                         end
                     end,
                     ProcInfo),
        PI;
       true ->
        lists:map(fun(PI) -> PI#etop_proc_info{runtime='-'} end,ProcInfo)
    end,
  ProcInfo2 = sort(Opts,ProcInfo1),
  Info#etop_info{procinfo=ProcInfo2}.

sort(Opts,PI) ->
  Tag = get_tag(Opts#opts.sort),
  PI1 = if Opts#opts.accum ->
            PI;
           true ->
            AccumTab = Opts#opts.accum_tab,
            lists:map(
              fun(#etop_proc_info{pid=Pid,reds=Reds,runtime=RT}=I) ->
                  NewI =
                    case ets:lookup(AccumTab,Pid) of
                      [#etop_proc_info{reds=OldReds,
                                       runtime='-'}] ->
                        I#etop_proc_info{reds=Reds-OldReds,
                                         runtime='-'};
                      [#etop_proc_info{reds=OldReds,
                                       runtime=OldRT}] ->
                        I#etop_proc_info{reds=Reds-OldReds,
                                         runtime=RT-OldRT};
                      [] ->
                        I
                    end,
                  ets:insert(AccumTab,I),
                  NewI
              end,
              PI)
        end,
  PI2 = lists:reverse(lists:keysort(Tag,PI1)),
  lists:sublist(PI2,Opts#opts.lines).

get_tag(runtime) -> #etop_proc_info.runtime;
get_tag(memory) -> #etop_proc_info.mem;
get_tag(reductions) -> #etop_proc_info.reds;
get_tag(msg_q) -> #etop_proc_info.mq.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Configuration Management

getopt(What, Config) when is_record(Config, opts) ->
  case What of
    node  -> Config#opts.node;
    port  -> Config#opts.port;
    accum -> Config#opts.accum;
    intv  -> Config#opts.intv;
    lines  -> Config#opts.lines;
    sort -> Config#opts.sort;
    width -> Config#opts.width;
    height-> Config#opts.height;

    store -> Config#opts.store;
    host  -> Config#opts.host
  end.

putopt(Key, Value, Config) when is_record(Config, opts) ->
  Config1 = handle_args([{Key,Value}],Config),
  Config1.

handle_args([{node, [NodeString]}| R], Config) when is_list(NodeString) ->
  Node = list_to_atom(NodeString),
  NewC = Config#opts{node = Node},
  handle_args(R, NewC);
handle_args([{node, Node} |R], Config) when is_atom(Node) ->
  NewC = Config#opts{node = Node},
  handle_args(R, NewC);
handle_args([{port, Port}| R], Config) when is_integer(Port) ->
  NewC = Config#opts{port=Port},
  handle_args(R, NewC);
handle_args([{port, [Port]}| R], Config) when is_list(Port) ->
  NewC = Config#opts{port= list_to_integer(Port)},
  handle_args(R, NewC);
handle_args([{interval, Time}| R], Config) when is_integer(Time)->
  NewC = Config#opts{intv=Time*1000},
  handle_args(R, NewC);
handle_args([{interval, [Time]}| R], Config) when is_list(Time)->
  NewC = Config#opts{intv=list_to_integer(Time)*1000},
  handle_args(R, NewC);
handle_args([{lines, Lines}| R], Config) when is_integer(Lines) ->
  NewC = Config#opts{lines=Lines},
  handle_args(R, NewC);
handle_args([{lines, [Lines]}| R], Config) when is_list(Lines) ->
  NewC = Config#opts{lines= list_to_integer(Lines)},
  handle_args(R, NewC);
handle_args([{accumulate, Bool}| R], Config) when is_atom(Bool) ->
  NewC = Config#opts{accum=Bool},
  handle_args(R, NewC);
handle_args([{accumulate, [Bool]}| R], Config) when is_list(Bool) ->
  NewC = Config#opts{accum= list_to_atom(Bool)},
  handle_args(R, NewC);
handle_args([{sort, Sort}| R], Config) when is_atom(Sort) ->
  NewC = Config#opts{sort=Sort},
  handle_args(R, NewC);
handle_args([{sort, [Sort]}| R], Config) when is_list(Sort) ->
  NewC = Config#opts{sort= list_to_atom(Sort)},
  handle_args(R, NewC);
handle_args([{tracing, OnOff}| R], Config) when is_atom(OnOff) ->
  NewC = Config#opts{tracing=OnOff},
  handle_args(R, NewC);
handle_args([{tracing, [OnOff]}| R], Config) when is_list(OnOff) ->
  NewC = Config#opts{tracing=list_to_atom(OnOff)},
  handle_args(R, NewC);

handle_args([_| R], C) ->
  handle_args(R, C);
handle_args([], C) ->
  C.

loadinfo(SysI) ->
  #etop_info{n_procs = Procs,
             run_queue = RQ,
             now = Now,
             wall_clock = {_, WC},
             runtime = {_, RT}} = SysI,
  Cpu = round(100*RT/WC),
  Clock = io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
                        tuple_to_list(element(2,calendar:now_to_datetime(Now)))),
  {Cpu,Procs,RQ,Clock}.

meminfo(MemI, [Tag|Tags]) ->
  [round(get_mem(Tag, MemI)/1024)|meminfo(MemI, Tags)];
meminfo(_MemI, []) -> [].

get_mem(Tag, MemI) ->
  case lists:keysearch(Tag, 1, MemI) of
    {value, {Tag, I}} -> I; %these are in bytes
    _ -> 0
  end.

update_json(Info, Config) ->
  {Cpu,NProcs,RQ,Clock} = loadinfo(Info),
  Header =
    case Info#etop_info.memi of
      undefined ->
        [{<<"node">>, Config#opts.node},
         {<<"clock">>, iolist_to_binary(Clock)},
         {<<"cpu">>, Cpu},
         {<<"nprocs">>, NProcs},
         {<<"runqueue">>, RQ}];
      Memi ->
        [Tot,Procs,Atom,Bin,Code,Ets] =
          etop2:meminfo(Memi, [total,processes,atom,binary,code,ets]),
        [{<<"node">>, Config#opts.node},
         {<<"clock">>, iolist_to_binary(Clock)},
         {<<"cpu">>, Cpu},
         {<<"tot">>, Tot},
         {<<"bin">>, Bin},
         {<<"nprocs">>, NProcs},
         {<<"procs">>, Procs},
         {<<"code">>, Code},
         {<<"runqueue">>, RQ},
         {<<"atom">>, Atom},
         {<<"ets">>, Ets}]
    end,
  Ps = [etop_proc_info_to_json(P) || P <- Info#etop_info.procinfo],
  [{<<"header">>, Header},
   {<<"procs">>, Ps}].

formatmfa({M, F, A}) ->
  io_lib:format("~w:~w/~w",[M, F, A]).

to_list(Name) when is_atom(Name) -> atom_to_list(Name);
to_list({_M,_F,_A}=MFA) -> formatmfa(MFA).

etop_proc_info_to_json(
  #etop_proc_info{pid=Pid,
                  mem=Mem,
                  reds=Reds,
                  name=Name,
                  runtime=Time,
                  cf=MFA,
                  mq=MQ}) ->
  PInfo =
    [{<<"pid">>, Pid},
     {<<"name">>, to_list(Name)},
     {<<"time">>, Time},
     {<<"reds">>, Reds},
     {<<"mem">>, Mem},
     {<<"mq">>, MQ},
     {<<"mfa">>, formatmfa(MFA)}],
  {<<"pinfo">>, PInfo}.
