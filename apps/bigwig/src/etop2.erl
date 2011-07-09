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

%% Internal
-export([loadinfo/1, meminfo/2]).

-include("etop.hrl").
-include("etop_defs.hrl").

-define(change_at_runtime_config,[lines,sort,accumulate]).

-define(SERVER, etop2_server).

%% Usage of the erlang top program
%% Options are set as command line parameters as in -node a@host -..
%% or as parameter to etop:start([{node, a@host}, {...}])
%% Options are:
%%   node        atom       Required   The erlang node to measure
%%   port        integer    The used port, NOTE: due to a bug this program
%%                          will hang if the port is not avaiable
%%   accumulate  boolean    If true execution time is accumulated
%%   lines       integer    Number of displayed processes
%%   sort        runtime | reductions | memory | msg_q
%%                          What information to sort by
%%                          Default: runtime (reductions if tracing=off)
%%   tracing     on | off   etop uses the erlang trace facility, and thus
%%                          no other tracing is possible on the node while
%%                          etop is running, unless this option is set to
%%                          'off'. Also helpful if the etop tracing causes
%%                          too high load on the measured node.
%%                          With tracing off, runtime is not measured!
%%   setcookie   string     Only applicable on operating system command
%%                          line. Set cookie for the etop node, must be
%%                          same as the cookie for the measured node.
%%                          This is not an etop parameter

terminate(_Reason, State=#opts{tracing=on}) ->
  etop_tr:stop_tracer(State);
terminate(_Reason, _) ->
  ok.

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
  Config1 = #opts{node=Node} = handle_args(Opts, #opts{}),

  %% Connect to the node we want to look at
  %% TODO: Move the connect elsewhere so we can switch nodes
  case connect(Node) of
    ok  ->
      %% Maybe set up the tracing
      Config2 = setup_tracing(Config1),

      AccumTab = ets:new(accum_tab,
                         [set,public,{keypos,#etop_proc_info.pid}]),
      Config3 = Config2#opts{accum_tab=AccumTab},

      {ok, Config3};
    Err ->
      Err
  end.

check_runtime_tools_vsn(Node) ->
  case rpc:call(Node,observer_backend,vsn,[]) of
    {ok, Vsn} -> check_vsn(Vsn);
    _ -> {error, "faulty version of runtime_tools on remote node"}
  end.
check_vsn(_Vsn) -> ok.

%% Handle the incoming data

update() ->
  gen_server:call(?SERVER, update).

check_runtime_config(lines,L) when is_integer(L),L>0 -> ok;
check_runtime_config(sort,S)
  when S=:=runtime; S=:=reductions; S=:=memory; S=:=msg_q -> ok;
check_runtime_config(accumulate,A) when A=:=true; A=:=false -> ok;
check_runtime_config(_Key,_Value) -> error.

handle_call(update, _From, State) ->
  Info = update(State),
  Json = update_json(Info, State),
  {reply, Json, State};
handle_call({config, Key, Value}, _From, State) ->
  case check_runtime_config(Key, Value) of
    error ->
      {reply, error, State};
    _ ->
      State2 = putopt(Key, Value, State),
      {reply, ok, State2}
  end.

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

putopt(Key, Value, #opts{} = Config) ->
  handle_args([{Key, Value}], Config).

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

connect(Node) ->
  case net_adm:ping(Node) of
    pang when Node /= node() ->
      {error, "node connect failed"};
    _Pong ->
      check_runtime_tools_vsn(Node)
  end.

setup_tracing(Config=#opts{tracing=T, sort=S, node=N}) ->
  if T =:= on, N /= node() ->
      %% Cannot trace on current node since the tracer will
      %% trace itself
      etop_tr:setup_tracer(Config);
     true ->
      if S =:= runtime ->
          Config#opts{sort=reductions, tracing=off};
         true ->
          Config#opts{tracing=off}
      end
  end.
