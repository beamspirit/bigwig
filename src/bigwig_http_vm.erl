%%
%% show details on the VM, releases, apps, etc.
%%
-module(bigwig_http_vm).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-compile(export_all).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Body = jsx:term_to_json(all()),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


all() ->
    [{system_info, system_info()},
     {releases,    releases()},
     {applications,applications()}].


%% Funs to load data about aspects of the system, mochiJSON formatted:

system_info() ->
    Fmt = fun(V) when is_number(V) -> V ;
             (V) when is_list(V)   -> list_to_binary(V) ;
             (V) when is_atom(V)   -> V end,

    Keys = [
            process_limit,
            kernel_poll,
            logical_processors,
            otp_release,
            system_architecture
           ],

    [ {N, Fmt(erlang:system_info(N))} || N <- Keys ].


releases() ->
    Rels = release_handler:which_releases(),
    %% TODO parse out the name/version of the apps?
    FmtDeps = fun(L) -> [ list_to_binary(A) || A <- L ] end,
    [
        {list_to_binary(Name),
         [
          {version, list_to_binary(Version)},
          {status,  list_to_binary(atom_to_list(Status))},
          {deps,    FmtDeps(Deps)}
         ]}
        || {Name, Version, Deps, Status} <- Rels
    ].


applications() ->
    %% Treat loaded as (Loaded - Running)
    Which   = application:which_applications(),
    Loaded0 = application:loaded_applications(),
    Loaded  = lists:filter(fun({Name, _Desc, _Ver}) ->
                            lists:keyfind(Name, 1, Which) =:= false
                           end, Loaded0),

    Format  = fun(AppList) ->
                      [ [
                            {name, Name},
                            {description, list_to_binary(Desc)},
                            {version,     list_to_binary(Ver)} ]
                    || {Name, Desc, Ver} <- AppList
                 ]
              end,

    [
        {running, Format(Which)},
        {loaded,  Format(Loaded)}
    ].


