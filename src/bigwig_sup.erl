-module(bigwig_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %Pubsub      = ?CHILD(bigwig_pubsubhub, worker),
    %StatsSender = ?CHILD(bigwig_stats_sender, worker),
    % Http        = ?CHILD(bigwig_http, worker),
    %ErrMon      = ?CHILD(bigwig_error_handler_mon, worker),
    %Etop        = ?CHILD(etop2, worker),
    %AppMon      = ?CHILD(bigwig_appmon, worker),

    %Specs       = [ Pubsub,
    %                StatsSender,
    %                AppMon,
    %                ErrMon,
    %                Etop,
    %                Http
    %              ],
    
    Procs = [],
    {ok, {{one_for_one, 10, 10}, Procs}}.

