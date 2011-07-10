-module(bigwig_appmon).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([handle_call/3, code_change/3]).

-record(mnode, {
          name   :: atom(),
          status :: [alive|dead],
          pid    :: pid(), % info pid
          apps   :: [{pid(), atom(), list()}],
          load   :: {non_neg_integer(), non_neg_integer()}
         }).

-record(state, {
          mnodes = [] :: [#mnode{}]
         }).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

init([]) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    NodesOk = lists:filter(fun check_node/1, nodes()),
    Nodes = [node()|NodesOk],
    {ok, #state{
       mnodes = [monitor_node(N) || N <- Nodes]
      }}.

handle_call(nodes, _From, #state{mnodes = M} = State) ->
    {reply, [lists:zip(record_info(fields, mnode),
                       tl(tuple_to_list(X))) || X <- M], State};
handle_call(_Msg, _From, State) ->
    {reply, badarg, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({statistics, L}, State) ->
    send({statistics, L}),
    {noreply, State};
handle_info({node_apps, _} = Msg, State) ->
    send(Msg),
    {noreply, State};
handle_info({node_app_tree, _} = Msg, State) ->
    send(Msg),
    {noreply, State};
handle_info({nodeup, Node}, State) ->
    case check_node(Node) of
        true  ->
            case get_mnode(Node, State#state.mnodes) of
                false -> send({new_node, Node});
                true  -> ok
            end,
            MNode = monitor_node(Node),
            MNodes = replace_mnode(Node, MNode, State#state.mnodes),
            send({node_up, Node}),
            {noreply, State#state{mnodes = MNodes}};
        false ->
            {noreply, State}
    end;
handle_info({nodedown, Node}, State) ->
    case get_mnode(Node, State#state.mnodes) of
        false ->
            {noreply, State};
        MNode ->
            MNode1 = MNode#mnode{status = dead},
            MNodes = replace_mnode(Node, MNode1, State#state.mnodes),
            send({node_down, Node}),
            {noreply, State#state{mnodes = MNodes}}
    end;
handle_info({'EXIT', _Pid, Reason}, State) ->
    case Reason of
        shutdown -> {stop, Reason, State};
        _ -> {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Make sure we have this module on the remote node
-spec check_node(atom()) -> boolean().
check_node(Node) -> is_list(rpc:call(Node, code, which, [?MODULE])).

-spec monitor_node(atom()) -> #mnode{}.
monitor_node(Node) ->
    {ok, Pid} = bigwig_appmon_info:start_link(Node, self()),
    #mnode{name = Node, status = alive, pid = Pid}.

-spec get_mnode(atom(), [#mnode{}]) -> #mnode{} | false.
get_mnode(Node, MNodes) ->
    case lists:keysearch(Node, #mnode.name, MNodes) of
        {value, MNode} -> MNode;
        false          -> false
    end.

-spec replace_mnode(atom(), #mnode{}, [#mnode{}]) -> [#mnode{}].
replace_mnode(Node, MNode, [#mnode{name=Node} | MNodes]) ->
    [MNode | MNodes];
replace_mnode(Node, MNode, [MNode2 | MNodes]) ->
    [MNode2 | replace_mnode(Node, MNode, MNodes)];
replace_mnode(_Node, MNode, []) ->
    [MNode].

-spec send(any()) -> ok.
send(Term) -> bigwig_pubsubhub:notify({?MODULE, Term}).
