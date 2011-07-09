-module(bigwig_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Http        = ?CHILD(bigwig_http, worker),
    ErrMon      = ?CHILD(bigwig_error_handler_mon, worker),
    Etop        = ?CHILD(etop2, worker),

    Specs       = [ ErrMon, Http, Etop ],

    {ok, { {one_for_one, 5, 10}, Specs} }.

