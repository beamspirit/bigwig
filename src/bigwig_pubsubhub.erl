-module(bigwig_pubsubhub).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, notify/1, register_client/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-record(state, {listeners=[]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

notify(Msg) ->
    gen_server:cast(?SERVER, {notify, Msg}).

register_client(Pid) ->
    gen_server:call(?SERVER, {register_client, Pid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({register_client, Pid}, _From, State) ->
    NewListeners = [ Pid | State#state.listeners ],
    erlang:monitor(process, Pid),
    {reply, ok, State#state{ listeners=NewListeners }}.

handle_cast({notify, Msg}, State) ->
    %%io:format("BROADCASTING: ~p\n", [Msg]),
    lists:foreach(fun(P) -> P ! {bigwig, Msg} end, State#state.listeners),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    NewListeners = lists:delete(Pid, State#state.listeners),
    {noreply, State#state{ listeners=NewListeners }};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

