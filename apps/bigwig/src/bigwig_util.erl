%%
%% miscellaneous utilities
%%
-module(bigwig_util).

-export([parse_term/1, ensure_dot/1]).

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
