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
-export([encode/1,
        to_jiffy/1]).


%%%===================================================================
%%% API
%%%===================================================================
encode(Data) ->
    Jiffy=to_jiffy(Data),
    jiffy:encode(Jiffy).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%%%===================================================================
%%% Internal functions
%%%===================================================================
to_jiffy(Tuple) when is_tuple(Tuple) ->
    to_jiffy_list(tuple_to_list(Tuple),[]);
to_jiffy(Port) when is_port(Port) ->
    list_to_binary(erlang:port_to_list(Port));
to_jiffy(Ref) when is_reference(Ref) ->
    list_to_binary(erlang:ref_to_list(Ref));            
to_jiffy(Pid) when is_pid(Pid) ->
    list_to_binary(erlang:pid_to_list(Pid));
to_jiffy(Values) when is_list(Values) ->
    to_jiffy_list(Values, []);
to_jiffy(Val) ->
    Val.


to_jiffy_list([], Acc) ->
    lists:reverse(Acc);
to_jiffy_list([Val | Rest], Acc0) ->
    Acc = [to_jiffy(Val) | Acc0],
    to_jiffy_list(Rest, Acc).




