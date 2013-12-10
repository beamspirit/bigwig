-module(bigwig_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    
    Dispatch = cowboy_router:compile([
        %% {URIHost, list({URIPath, Handler, Opts})}
        {'_', [
               {"/",              cowboy_static,        {priv_file, bigwig, "html/index.html"}},
               {"/static/[...]",  cowboy_static,        {priv_dir,  bigwig, "",
                                                         [{mimetypes, cow_mimetypes, all}]}},
               {"/vm",              bigwig_http_vm,       []},
               {"/rb/stream",       bigwig_http_rb_stream, []},               
               {"/rb/[...]",        bigwig_http_rb, []},
               {"/pid/[...]",        bigwig_http_pid, []},
               {"/top/[...]",             bigwig_http_etop2,    []},
               {"/entop/[...]",             bigwig_http_entop,    []},
               {"/appmon/[...]",          bigwig_http_appmon,   []},
               {"/module/[...]",    bigwig_http_module,   []},
               {"/stats-stream",    bigwig_http_stats_stream, []},
               {"/lager/stream",    bigwig_http_lager_stream, []},
               {"/lager/[...]",        bigwig_http_lager, []},
               {'_',                bigwig_http_catchall, []}
              ]}
    ]),
    
    Port = 40829,
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(my_http_listener, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]
    ),

    lager:info("Bigwig listening on port ~B", [Port]),
    bigwig_sup:start_link().

stop(_State) ->
    ok.
