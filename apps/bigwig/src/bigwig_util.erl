%%
%% miscellaneous utilities
%%
-module(bigwig_util).

-export([parse_term/1, ensure_dot/1, url_decode/1]).
-export([date_str/1, date_str/2]).

-spec parse_term(binary() | list()) -> {ok, any()} | {error, any()}.
parse_term(Bin) when is_binary(Bin) ->
    parse_term(binary_to_list(Bin));
parse_term(Str) when is_list(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens0, _EndLoc} ->
            Tokens = ensure_dot(Tokens0),
            erl_parse:parse_term(Tokens)
    end.

-spec ensure_dot(erl_scan:tokens()) -> erl_scan:tokens().
ensure_dot(Tokens) ->
    case lists:last(Tokens) of
        {dot, 1} -> Tokens;
        _        -> Tokens ++ [{dot, 1}]
    end.

%% the following url encoding stuff was taken from yaws.erl
%% sadly, cowboy _req:qs_val doesn't url-decode vals
%% http://drproxy.googlecode.com/svn/trunk/extlib/yaws-1.68/src/yaws.erl
-spec url_decode(binary() | list()) -> list().
url_decode(Bin) when is_binary(Bin) ->
    url_decode(binary_to_list(Bin));
url_decode([$%, Hi, Lo | Tail]) ->
            Hex = erlang:list_to_integer([Hi, Lo], 16),
            [Hex | url_decode(Tail)];
            url_decode([$?|T]) ->
                   [$?|T];
            url_decode([H|T]) when is_integer(H) ->
                   [H |url_decode(T)];
            url_decode([]) ->
                   [];
            %% deep lists
            url_decode([H|T]) when is_list(H) ->
                   [url_decode(H) | url_decode(T)].
date_str({Date,Time}) ->
  date_str(Date, Time).
date_str({Y,Mo,D}=Date,{H,Mi,S}=Time) ->
    case application:get_env(sasl,utc_log) of 
	{ok,true} ->
	    {{YY,MoMo,DD},{HH,MiMi,SS}} = 
		local_time_to_universal_time({Date,Time}),
	    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
					"~2.2.0w:~2.2.0w UTC", 
					[YY,MoMo,DD,HH,MiMi,SS]));
	_ ->
	    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
					"~2.2.0w:~2.2.0w", 
					[Y,Mo,D,H,Mi,S]))
    end.

local_time_to_universal_time({Date,Time}) ->
    case calendar:local_time_to_universal_time_dst({Date,Time}) of
	[UCT] ->
	    UCT;
	[UCT1,_UCT2] ->
	    UCT1;
	[] -> % should not happen
	    {Date,Time}
    end.

