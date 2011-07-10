%%
%% Used to add and catch crashes of our own error handler
%%
-module(bigwig_error_handler_mon).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop()       -> gen_event:call(?MODULE, stop, infinity).

%%%----------------------------------------------------------------------------

init([]) ->
    %% Add our error handler
    ok = bigwig_error_handler:add_sup_handler(),
    {ok, #state{}}.

handle_call(stop, _From, State)     -> {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {reply, not_implemented, State}.

handle_cast(_Msg, State)            -> {noreply, State}.

handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    %% Our handler crashed, so we crash, allowing supervisor to restart us
    %% which will re-add the handler.
    io:format("bigwig error handler ~p crashed: ~p (pid ~p)",
        [Handler, Reason, self()]),
    {stop, gen_event_EXIT, State};

handle_info(_Info, State)           -> {noreply, State}.

terminate(_Reason, _State)          -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

