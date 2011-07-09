-module(bigwig_http).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0]).

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

dispatch_rules() ->
    %% {Host, list({Path, Handler, Opts})}
    [{'_', [
        
            {[<<"">>],                  bigwig_http_index, []}

        ,   {[<<"static">>, '...'],     bigwig_http_static, []}
        ,   {[<<"wh">>],                bigwig_http_static, ["html","index.html"]}
        ,   {[<<"wh">>, <<"ws">>],      bigwig_http_wh, []}
        ,   {[<<"vm">>],                bigwig_http_vm, []}
        ,   {[<<"rb">>, '...'],         bigwig_http_rb, []}

        ,   {'_',                       bigwig_http_catchall, []}
    ]}].

init([]) ->
    Port = 8080,
    error_logger:info_msg("Starting http server on port ~p", [Port]),
    %%
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port, Port}],
        cowboy_http_protocol, [{dispatch, dispatch_rules()}]
    ),

    {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

