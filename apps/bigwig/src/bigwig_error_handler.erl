%%
%% Gets added as a handler to SASL's error_logger
%% Broadcasts SASL reports to all registered client pids
%%
%% Used to stream new reports down a websocket.
%%
-module(bigwig_error_handler).
-behaviour(gen_event).

-export([add_sup_handler/0, register_client/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listeners=[]}).

%%%----------------------------------------------------------------------------

add_sup_handler() -> 
    gen_event:add_sup_handler(error_logger, ?MODULE, []).

register_client(Pid) ->
    gen_event:call(error_logger, ?MODULE, {register_client, Pid}).

%%%----------------------------------------------------------------------------

init([]) -> {ok, #state{}}.

handle_event(Event, State) ->
    %%io:format("EVENT(to ~p) ~p\n", [length(State#state.listeners), Event]),
    Msg = {?MODULE, Event},
    lists:foreach(fun(Pid) -> Pid ! Msg end, State#state.listeners),
    {ok, State}.

handle_call({register_client, Pid}, State) ->
    NewListeners = [ Pid | State#state.listeners ],
    erlang:monitor(process, Pid),
    {ok, ok, State#state{ listeners=NewListeners }};

handle_call(_Msg, State)   -> {ok, not_implemented, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    NewListeners = lists:remove(Pid, State#state.listeners),
    {ok, ok, State#state{ listeners=NewListeners }};

handle_info(_Info, State)  -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

