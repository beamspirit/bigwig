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
-export([start_link/0, start_link/1, update/0, node/1, config/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% Internal
-export([loadinfo/1, meminfo/2]).

-include("etop.hrl").
-include("etop_defs.hrl").

-define(change_at_runtime_config,[lines,sort,accumulate]).

-define(SERVER, etop2_server).

-define(ACCUM_TAB, accum_tab).

-define(DEFAULT_LINES, 1000).

%% Options are set as parameters to etop2:start_link([{node, a@host}, {...}])
%%
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

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Opts) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

update() ->
  gen_server:call(?SERVER, update).

-spec node(atom()) -> ok.
node(Node) ->
  gen_server:call(?SERVER, {node, Node}).

-spec config(binary(), binary()) -> ok | error.
config(Key, Value) ->
  gen_server:call(?SERVER, {config, Key, Value}).

init(Opts) ->
  State1 = handle_args(Opts, #opts{lines=?DEFAULT_LINES}),
  AccumTab = ets:new(?ACCUM_TAB, [set,public,{keypos,#etop_proc_info.pid}]),
  State2 = State1#opts{accum_tab=AccumTab},
  Res = connect(State2),
  Res.

handle_cast(_Msg, State) ->
  {stop, unhandled_cast, State}.

handle_call(update, _From, State) ->
  Info = update(State),
  Json = update_json(Info, State),
  {reply, Json, State};
handle_call({config, Key, Value}, _From, State) ->
  case parse_config_value(Key, Value) of
    {ok, K, V} ->
      case check_runtime_config(K, V) of
        error ->
          {reply, error, State};
        _ ->
          State2 = putopt(K, V, State),
          {reply, ok, State2}
      end;
    _ ->
      {reply, error, State}
  end;
handle_call({node, Node}, _From, State=#opts{node=Node}) ->
  {reply, ok, State};
handle_call({node, Node}, _From, State) ->
  State2 = stop_tracing(State),
  case connect(State2#opts{node=Node}) of
    {ok, State3} ->
      clear_ets(State3),
      {reply, ok, State3};
    {error, Error} ->
      {reply, Error, State2}
  end.

handle_info(_Info, State) ->
  {stop, unhandled_info, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, State=#opts{tracing=on}) ->
  etop_tr:stop_tracer(State);
terminate(_Reason, _) ->
  ok.

%% Internal functions

check_runtime_tools_vsn(Node) ->
  case rpc:call(Node,observer_backend,vsn,[]) of
    {ok, Vsn} -> check_vsn(Vsn);
    _ -> {error, "faulty version of runtime_tools on remote node"}
  end.
check_vsn(_Vsn) -> ok.

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

putopt(Key, Value, #opts{} = State) ->
  handle_args([{Key, Value}], State).

handle_args([{node, [NodeString]}| R], State) when is_list(NodeString) ->
  Node = list_to_atom(NodeString),
  NewC = State#opts{node = Node},
  handle_args(R, NewC);
handle_args([{node, Node} |R], State) when is_atom(Node) ->
  NewC = State#opts{node = Node},
  handle_args(R, NewC);
handle_args([{port, Port}| R], State) when is_integer(Port) ->
  NewC = State#opts{port=Port},
  handle_args(R, NewC);
handle_args([{port, [Port]}| R], State) when is_list(Port) ->
  NewC = State#opts{port= list_to_integer(Port)},
  handle_args(R, NewC);
handle_args([{lines, Lines}| R], State) when is_integer(Lines) ->
  NewC = State#opts{lines=Lines},
  handle_args(R, NewC);
handle_args([{lines, [Lines]}| R], State) when is_list(Lines) ->
  NewC = State#opts{lines= list_to_integer(Lines)},
  handle_args(R, NewC);
handle_args([{accumulate, Bool}| R], State) when is_atom(Bool) ->
  NewC = State#opts{accum=Bool},
  handle_args(R, NewC);
handle_args([{accumulate, [Bool]}| R], State) when is_list(Bool) ->
  NewC = State#opts{accum= list_to_atom(Bool)},
  handle_args(R, NewC);
handle_args([{sort, Sort}| R], State) when is_atom(Sort) ->
  NewC = State#opts{sort=Sort},
  handle_args(R, NewC);
handle_args([{sort, [Sort]}| R], State) when is_list(Sort) ->
  NewC = State#opts{sort= list_to_atom(Sort)},
  handle_args(R, NewC);
handle_args([{tracing, OnOff}| R], State) when is_atom(OnOff) ->
  NewC = State#opts{tracing=OnOff},
  handle_args(R, NewC);
handle_args([{tracing, [OnOff]}| R], State) when is_list(OnOff) ->
  NewC = State#opts{tracing=list_to_atom(OnOff)},
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

update_json(Info, #opts{node=Node}) ->
  {Cpu,NProcs,RQ,Clock0} = loadinfo(Info),
  Clock = iolist_to_binary(Clock0),
  Header =
    case Info#etop_info.memi of
      undefined ->
        [{<<"node">>, Node},
         {<<"clock">>, Clock},
         {<<"cpu">>, Cpu},
         {<<"nprocs">>, NProcs},
         {<<"runqueue">>, RQ}];
      Memi ->
        [Tot,Procs,Atom,Bin,Code,Ets] =
          etop2:meminfo(Memi, [total,processes,atom,binary,code,ets]),
        [{<<"node">>, Node},
         {<<"clock">>, Clock},
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
  NumPs = length(Ps),
  [{<<"iTotalRecords">>, NumPs},
   {<<"iTotalDisplayRecords">>, NumPs},
   {<<"sColumns">>, <<"pid,name,time,reds,mem,mq,mfa">>},
   {<<"aaData">>, Ps}].

name(Name) when is_atom(Name) -> Name;
name({M,F,A}) -> [M, F, A].

etop_proc_info_to_json(
  #etop_proc_info{pid=Pid,
                  mem=Mem,
                  reds=Reds,
                  name=Name,
                  runtime=Time,
                  cf=MFA,
                  mq=MQ}) ->
  [list_to_binary(pid_to_list(Pid)),name(Name),Time,Reds,Mem,MQ,name(MFA)].

%% Connect to a node and potentially set up tracing
-spec connect(#opts{}) -> {ok, #opts{}} | {error, any()}.
connect(State=#opts{node=Node}) ->
  case net_adm:ping(Node) of
    pang when Node /= node() ->
      {error, "node connect failed"};
    _Pong ->
      case check_runtime_tools_vsn(Node) of
        ok             ->
          {ok, setup_tracing(State)};
        {error, _} = E ->
          E
      end
  end.

-spec setup_tracing(#opts{}) -> #opts{}.
setup_tracing(State=#opts{tracing=T, sort=S, node=N}) ->
  if T =:= on, N /= node() ->
      %% Cannot trace on current node since the tracer will
      %% trace itself
      etop_tr:setup_tracer(State);
     true ->
      if S =:= runtime ->
          State#opts{sort=reductions, tracing=off};
         true ->
          State#opts{tracing=off}
      end
  end.

-spec stop_tracing(#opts{}) -> #opts{}.
stop_tracing(State=#opts{tracing=on}) ->
  etop_tr:stop_tracer(State);
stop_tracing(State) ->
  State.

-spec clear_ets(#opts{}) -> ok | {error, any()}.
clear_ets(#opts{accum_tab=AccumTab}) ->
  ets:delete_all_objects(AccumTab).

check_runtime_config(lines,L) when is_integer(L),L>0 -> ok;
check_runtime_config(sort,S)
  when S=:=runtime; S=:=reductions; S=:=memory; S=:=msg_q -> ok;
check_runtime_config(accumulate,A) when A=:=true; A=:=false -> ok;
check_runtime_config(_Key,_Value) -> error.

-spec parse_config_value(binary(), binary()) -> {ok, atom() | integer()} | error.
parse_config_value(K, V) ->
  case {config_key(K), config_value(V)} of
    {{ok, Key}, {ok, Value}} ->
      {ok, Key, Value};
    _ ->
      error
  end.

-spec config_key(binary()) -> {ok, atom()} | error.
config_key(K) ->
  binary_to_existing_atom(K).

-spec config_value(binary()) -> {ok, atom() | integer()} | error.
config_value(V) ->
  case binary_to_existing_atom(V) of
    error ->
      try
        {ok, list_to_integer(binary_to_list(V))}
      catch _:_ ->
          error
      end;
    Res -> Res
  end.

-spec binary_to_existing_atom(binary()) -> {ok, atom()} | error.
binary_to_existing_atom(B) ->
  try
    {ok, list_to_existing_atom(binary_to_list(B))}
  catch _:_ ->
      error
  end.
