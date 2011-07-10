%% Periodically broadcasts stats about the VM to clients
%%
-module(bigwig_stats_sender).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(INTERVAL, 2000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    timer:send_interval(?INTERVAL, calculate),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(calculate, State) ->
    Msg = {bigwig_stats_sender, calc_stats()},
    bigwig_pubsubhub:notify(Msg),
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% Generate a proplist of jsx_to_term compatible data about VM state
calc_stats() ->
    {{Y,M,D},{Hr,Sec,Min}} = erlang:universaltime(),
    {{input, InBytes}, {output, OutBytes}} = erlang:statistics(io),
    {RedsTot, _RedsDiff} = erlang:statistics(reductions),
    {Uptime , _} = erlang:statistics(wall_clock),

    [
        {process_count, erlang:system_info(process_count)},
        {date, [Y,M,D,Hr,Sec,Min]},
        {reductions, RedsTot},
        {bytes_in, InBytes},
        {bytes_out, OutBytes},
        {run_queue, erlang:statistics(run_queue)},
        {uptime, Uptime}
    ].
