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
encode(Data) ->
    encode(Data, []).


encode(Data, Options) ->
    ForceUTF8 = lists:member(force_utf8, Options),
    case nif_encode(Data, Options) of
        {error, invalid_string} when ForceUTF8 == true ->
            FixedData = fix(Data),
            encode(FixedData, Options -- [force_utf8]);
        {error, invalid_object_pair} ->
            fix(Data);
        {error, _} = Error ->
            throw(Error);
        {partial, IOData} ->
            jiffy:finish_encode(IOData, []);
        IOData ->
            IOData
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%%%===================================================================
%%% Internal functions
%%%===================================================================
fix(Values) when is_list(Values) ->
    fix_array(Values, []);

fix({K, V}) ->
    {fix(K), fix(V)};
fix(Tuple) when is_tuple(Tuple) ->
    to_jiffy_list(tuple_to_list(Tuple),[]);
fix(Port) when is_port(Port) ->
    list_to_binary(erlang:port_to_list(Port));
fix(Ref) when is_reference(Ref) ->
    list_to_binary(erlang:ref_to_list(Ref));            
fix(Pid) when is_pid(Pid) ->
    list_to_binary(erlang:pid_to_list(Pid));
fix(Other) ->
    jiffy_utf8:fix(Other).


fix_array([], Acc) ->
    lists:reverse(Acc);
fix_array([Val | Rest], Acc0) ->
    Acc = [fix(Val) | Acc0],
    fix_array(Rest, Acc).
