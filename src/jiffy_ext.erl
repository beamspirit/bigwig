%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2013, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(jiffy_ext).

%% API
-export([encode/1]).

%%%===================================================================
%%% API
%%%===================================================================
encode(Binary) when is_binary(Binary) ->
    jiffy_ext:encode(Binary);
encode(Atom) when is_atom(Atom) ->
    jiffy:encode(Atom);
encode(Pid) when is_pid(Pid) ->
    jiffy:encode(list_to_binary(erlang:pid_to_list(Pid)));
encode(Port) when is_port(Port) ->
    jiffy:encode(list_to_binary(erlang:port_to_list(Port)));
encode(Ref) when is_reference(Ref) ->
    jiffy:encode(list_to_binary(erlang:ref_to_list(Ref)));
encode({{Y,M,D},{Hrs,Mins,Secs}}) ->
    jiffy_ext:encode([Y,M,D,Hrs,Mins,Secs]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
