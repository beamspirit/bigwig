%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodinet.com>
%%% @copyright (C) 2013, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2013 by Jack Tang <jack@taodinet.com>
%%%-------------------------------------------------------------------
-module(md_amqp_subscriber).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE). 
-include_lib("amqp_client/include/amqp_client.hrl").


-record(state, {node_sub_count, %orddict {key:Node, value:Count}
                node_sub_detail, %orddict {key:InvestorId, value:{Node, Time}}
                node_client_instrument_count}). %orddict {key:{Node, Time}, value:{ClientCount, InstrumentCount}

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Params], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Params]) ->
    Exchange = config_val(exchange, Params, <<"market_subscriber_statistic">>),
    AmqpParams = #amqp_params_network {
      username       = config_val(amqp_user, Params, <<"guest">>),
      password       = config_val(amqp_pass, Params, <<"guest">>),
      virtual_host   = config_val(amqp_vhost, Params, <<"/">>),
      host           = config_val(amqp_host, Params, "localhost"),
      port           = config_val(amqp_port, Params, 5672)
     },

    {ok, Channel} = amqp_channel(AmqpParams),

    #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{ exchange = Exchange, 
                                                                               type = <<"topic">>, durable = true }),

    %% Declare a queue
    #'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, #'queue.declare'{queue = <<"md_stat">>, durable = true}),
    Binding = #'queue.bind'{queue = Q, exchange = Exchange, routing_key = <<"md_stat">>},
     #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    Sub = #'basic.consume'{queue = Q},
    % Subscribe the channel and consume the message
    Consumer = self(),
    #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, Sub, Consumer),

    {ok, #state{node_client_instrument_count = orddict:new()}}. %orddict {key:{Node, Time}, value:{ClientCount, InstrumentCount}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    lager:warning("Can't handle request: ~p~n", [_Request]),
    {reply, {error, invalid_req}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    lager:warning("Can't handle msg: ~p~n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info(#'basic.cancel_ok'{}, State) ->
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = _Tag}, 
    {_, _, Message} = _Content}, #state{} = State) ->
    #state{node_client_instrument_count  = NodeClientInstrumentCount} = State,
    [Conn0, Node0, Time0] = binary:split(Message, <<" ">>, [global]),
    Conn  = binary_to_atom(Conn0, utf8),
    Node  = binary_to_atom(Node0, utf8),
    Time = list_to_integer(binary_to_list(Time0)),
    {LastHourClientCount, LastHourInstrumentCount} = 
        case orddict:find({Node, Time - 1}, NodeClientInstrumentCount) of
            error -> {0, 0};
            {ok, Value} -> Value
        end,
    NodeClientInstrumentCount0 = 
        case Conn of
            connected ->
                NodeClientInstrumentCount1 =          
                    case orddict:find({Node, Time}, NodeClientInstrumentCount) of
                        error -> 
                            orddict:store({Node, Time}, {LastHourClientCount + 1, LastHourInstrumentCount}, 
                                NodeClientInstrumentCount);
                        {ok, {CValue, IValue}}  -> 
                            orddict:store({Node, Time}, {CValue + 1, IValue}, NodeClientInstrumentCount)
                    end,
                NodeClientInstrumentCount1;
            disconnected ->
                NodeClientInstrumentCount1 =          
                    case orddict:find({Node, Time}, NodeClientInstrumentCount) of
                        error -> 
                            orddict:store({Node, Time}, {LastHourClientCount, LastHourInstrumentCount}, 
                                NodeClientInstrumentCount);
                        {ok, {CValue, IValue}}  -> 
                            orddict:store({Node, Time}, {CValue - 1, IValue}, NodeClientInstrumentCount)
                    end,
                NodeClientInstrumentCount1;
            subscribe ->
                NodeClientInstrumentCount1 =          
                    case orddict:find({Node, Time}, NodeClientInstrumentCount) of
                        error -> 
                            orddict:store({Node, Time}, {LastHourClientCount, LastHourInstrumentCount + 1}, 
                                NodeClientInstrumentCount);
                        {ok, {CValue, IValue}}  -> 
                            orddict:store({Node, Time}, {CValue, IValue + 1}, NodeClientInstrumentCount)
                    end,
                NodeClientInstrumentCount1;
            unsubscribe ->
                NodeClientInstrumentCount1 =          
                    case orddict:find({Node, Time}, NodeClientInstrumentCount) of
                        error -> 
                            orddict:store({Node, Time}, {LastHourClientCount, LastHourInstrumentCount}, 
                                NodeClientInstrumentCount);
                        {ok, {CValue, IValue}}  -> 
                            orddict:store({Node, Time}, {CValue, IValue - 1}, NodeClientInstrumentCount)
                    end,
                NodeClientInstrumentCount1
        end,
    BigwigMsg = lists:map(fun(A) -> {NodeKey, NodeValue}=A,erlang:tuple_to_list(NodeKey) ++ 
        erlang:tuple_to_list(NodeValue) end, 
        orddict:to_list(NodeClientInstrumentCount0)),
    Msg={market_dispatcher, BigwigMsg},
    bigwig_pubsubhub:notify(Msg),
    {noreply, State#state{node_client_instrument_count = NodeClientInstrumentCount0}};

handle_info(_Info, State) ->
    lager:warning("Can't handle info: ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


amqp_channel(AmqpParams) ->
    case maybe_new_pid({AmqpParams, connection},
                       fun() -> amqp_connection:start(AmqpParams) end) of
        {ok, Client} ->
            maybe_new_pid({AmqpParams, channel},
                          fun() -> amqp_connection:open_channel(Client) end);
        Error ->
            Error
    end.

maybe_new_pid(Group, StartFun) ->
    case pg2:get_closest_pid(Group) of
        {error, {no_such_group, _}} ->
            pg2:create(Group),
            maybe_new_pid(Group, StartFun);
        {error, {no_process, _}} ->
            case StartFun() of
                {ok, Pid} ->
                    pg2:join(Group, Pid),
                    {ok, Pid};
                Error ->
                    Error
            end;
        Pid ->
            {ok, Pid}
    end.


config_val(C, Params, Default) ->
  case lists:keyfind(C, 1, Params) of
    {C, V} -> V;
    _ -> Default
  end.
