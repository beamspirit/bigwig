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

-module(etop2_json).
-author('siri@erix.ericsson.se').

-export([init/1,stop/1]).
-export([do_update/3]).

-include_lib("etop.hrl").
-include("etop_defs.hrl").

stop(Pid) -> Pid ! stop.

init(Config) ->
  loop(Config).

loop(Config) ->
  Info = do_update(Config),
  receive
    stop -> stopped;
    {dump,Fd} -> do_update(Fd,Info,Config), loop(Config);
    {config,_,Config1} -> loop(Config1)
  after Config#opts.intv -> loop(Config)
  end.

do_update(Config) ->
  Info = etop:update(Config),
  do_update(standard_io,Info,Config).

do_update(Fd,Info,Config) ->
  {Cpu,NProcs,RQ,Clock} = etop2:loadinfo(Info),
  Header =
    case Info#etop_info.memi of
      undefined ->
        H =
          [{<<"node">>, Config#opts.node},
           {<<"clock">>, iolist_to_binary(Clock)},
           {<<"cpu">>, Cpu},
           {<<"nprocs">>, NProcs},
           {<<"runqueue">>, RQ}],
        {<<"header">>, H};
      Memi ->
        [Tot,Procs,Atom,Bin,Code,Ets] =
          etop2:meminfo(Memi, [total,processes,atom,binary,code,ets]),
        H =
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
           {<<"ets">>, Ets}],
        {<<"header">>, H}
    end,
  Ps = [etop_proc_info_to_json(P) || P <- Info#etop_info.procinfo],
  io:fwrite(Fd, "~p", [Header]),
  io:fwrite(Fd, "~p", [Ps]),
  Info.

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
