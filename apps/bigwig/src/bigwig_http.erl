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

            {[],                       bigwig_http_static, [<<"html">>,<<"index.html">>]}
        ,   {[<<"static">>, '...'],     bigwig_http_static, []}
        ,   {[<<"wh">>, <<"ws">>],      bigwig_http_wh, []}
        ,   {[<<"vm">>],                bigwig_http_vm, []}
        ,   {[<<"rb">>],                bigwig_http_static, [<<"html">>,<<"rb.html">>]}
        ,   {[<<"rb">>, <<"stream">>],  bigwig_http_rb_stream, []}
        ,   {[<<"rb">>, '...'],         bigwig_http_rb, []}
        ,   {[<<"pid">>, '...'],        bigwig_http_pid, []}
        ,   {[<<"module">>, '...'],     bigwig_http_module, []}
        ,   {[<<"et">>],                bigwig_http_static, [<<"html">>,<<"et.html">>]}
        ,   {[<<"top">>, '...'],        bigwig_http_etop2, []}
        ,   {[<<"appmon">>],            bigwig_http_static, [<<"html">>,<<"appmon.html">>]}
        ,   {[<<"appmon">>, '...'],     bigwig_http_appmon, []}
        ,   {[<<"stats-stream">>],      bigwig_http_stats_stream, []}
        ,   {'_',                       bigwig_http_catchall, []}
    ]}].

confval(Key, Default) ->
    case application:get_env(Key) of
        undefined -> Default;
        Val       -> Val
    end.

init([]) ->
    Port            = confval(port, 9999),
    Ip              = confval(ip, "127.0.0.1"),
    NumAcceptors    = confval(num_acceptors, 16),

    IpStr = case is_list(Ip) of true -> Ip; false -> inet_parse:ntoa(Ip) end,
    error_logger:info_msg("Bigwig listening on http://~s:~B/~n", [IpStr,Port]),
    %%
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(http, NumAcceptors,
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
