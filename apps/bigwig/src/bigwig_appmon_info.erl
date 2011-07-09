-module(bigwig_appmon_info).
-behaviour(gen_server).

-export([start_link/2, start_link_1/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([handle_call/3, code_change/3]).

-compile(export_all).

-record(state, {
          client :: pid(),
          tref :: reference()
         }).

-record(db, {q, p, links, links2}).

-define(IGNORE_APPS, [application_controller, init, error_logger, gs,
                      node_serv, appmon, appmon_a, appmon_info]).

start_link(Node, Client) ->
    rpc:call(Node, ?MODULE, start_link_1, [Client]).

start_link_1(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

stop(Name) ->
    gen_server:cast(Name, stop).

init([Client]) ->
    {ok, statistics_timer(#state{client = Client})}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, badarg, State}.

handle_info({timeout, Ref, statistics},
            #state{client = Pid, tref = Ref} = State) ->
    L = [{context_switches, statistics(context_switches)},
         {garbage_collection, statistics(garbage_collection)},
         {io, statistics(io)},
         {reductions, statistics(reductions)},
         {run_queue, statistics(run_queue)},
         {runtime, statistics(runtime)},
         {wall_clock, statistics(wall_clock)}],
    Pid ! {statistics, L},
    {noreply, statistics_timer(State)};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

statistics_timer(#state{tref = Ref} = S) ->
    case Ref of
        undefined -> ok;
        _ -> erlang:cancel_timer(Ref)
    end,
    S#state{tref = erlang:start_timer(1000, self(), statistics)}.

node_apps() ->
    {ok, lists:zf(
           fun(App) ->
                   Name = element(1, App),
                   case catch application_controller:get_master(Name) of
                       Pid when is_pid(Pid) -> {true, {Pid, Name, App}};
                       _ -> false
                   end
           end, application:which_applications())}.

calc_app_tree(Name) -> calc_app_tree(Name, link).

calc_app_tree(Name, Mode) ->
    case application_controller:get_master(Name) of
        Pid when is_pid(Pid) ->
            DB = new_db(Mode, Pid),
            GL = groupl(Pid),
            R = case catch do_find_proc(Mode, DB, GL, find_avoid()) of
                    {ok, DB2} ->
                        {ok, [{root, pn(Pid)},
                              {p, lists:zf(fun pid_info/1, ets:tab2list(DB2#db.p))},
                              {l, lrdb(DB2#db.links)},
                              {l2, lrdb(DB2#db.links2)}]};
                    {error, Reason} ->
                        {error, Reason};
                    Other ->
                        {error, Other}
                end,
            ets:delete(DB#db.p),
            ets:delete(DB#db.links),
            ets:delete(DB#db.links2),
            R;
        _ ->
            {ok, [{}]}
    end.

pn(Pid) when is_pid(Pid) -> list_to_binary(pid_to_list(Pid));
pn(Oth) -> Oth.

lrdb(Db) -> [[pn(Left), pn(Right)] || {Left, Right} <- ets:tab2list(Db)].

get_pid(P) when is_pid(P) -> P;
get_pid(P) when is_port(P) -> P;
get_pid(X) when is_tuple(X) -> element(2, X).


%----------------------------------------------------------------------
%%---------------------------------------------------------------------
%% Handling process trees of processses that are linked to each other

do_find_proc(Mode, DB, GL, Avoid) ->
    case get_next(DB) of
        {{value, V}, DB2} ->
            do_find_proc2(V, Mode, DB2, GL, Avoid);
        {empty, DB2} ->
            {ok, DB2}
    end.

do_find_proc2(X, Mode, DB, GL, Avoid) when is_port(X) ->
    %% There used to be a broken attempt here to handle ports,
    %% but the rest of appmon can't handle ports, so now we
    %% explicitly ignore ports.
    do_find_proc(Mode, DB, GL, Avoid);
do_find_proc2(X, Mode, DB, GL, Avoid) ->
    Xpid = get_pid(X),
    DB2 = case is_proc(DB, Xpid) of
              false ->
                  add_proc(DB, Xpid),
                  C1 = find_children(X, Mode),
                  add_children(C1, Xpid, DB, GL, Avoid, Mode);
              _ ->
                  DB
          end,
    do_find_proc(Mode, DB2, GL, Avoid).


%% Find children finds the children of a process. The method varies
%% with the selected mode (sup or link) and there are also some
%% processes that must be treated differently, notably the application
%% master.
%%
find_children(X, sup) when is_pid(X) ->
    %% This is the first (root) process of a supervision tree and it
    %% better be a supervisor, we are smoked otherwise
    supervisor:which_children(X);
find_children(X, link) when is_pid(X), node(X) /= node() ->
    [];
find_children(X, link) when is_pid(X) ->
    case process_info(X, links) of
        {links, Links} ->
            lists:reverse(Links); % OTP-4082
        _ -> []
    end;
find_children({master, X}, sup) ->
    case application_master:get_child(X) of
        {Pid, _Name} when is_pid(Pid) -> [Pid];
        Pid when is_pid(Pid) -> [Pid]
    end;
find_children({_, _X, worker, _}, sup) -> [];
find_children({_, X, supervisor, _}, sup) ->
    lists:filter(fun(Thing) ->
                         Pid = get_pid(Thing),
                         if
                             is_pid(Pid) -> true;
                             true -> false
                         end
                 end,
                 supervisor:which_children(X)).


%% Add links to primary (L1) or secondary (L2) sets and return an
%% updated queue. A link is considered secondary if its endpoint is in
%% the queue of un-visited but known processes.
add_children(CList, Paren, DB, _GL, _Avoid, sup) ->
    lists:foldr(fun(C, DB2) ->
                        case get_pid(C) of
                            P when is_pid(P) ->
                                add_prim(C, Paren, DB2);
                            _ -> DB2 end end,
                DB, CList);

add_children(CList, Paren, DB, GL, Avoid, _Mode) ->
    lists:foldr(fun(C, DB2) ->
                        maybe_add_child(C, Paren, DB2, GL, Avoid)
                end, DB, CList).

%% Check if the child is already in P
maybe_add_child(C, Paren, DB, GL, Avoid) ->
    case is_proc(DB, C) of
        false ->
            maybe_add_child_node(C, Paren, DB, GL, Avoid);
        _ -> DB					% In P: no action
    end.

%% Check if process on this node
maybe_add_child_node(C, Paren, DB, GL, Avoid) ->
    if
        node(C) /= node() ->
            add_foreign(C, Paren, DB);
        true ->
            maybe_add_child_avoid(C, Paren, DB, GL, Avoid)
    end.

%% Check if child is on the avoid list
maybe_add_child_avoid(C, Paren, DB, GL, Avoid) ->
    case lists:member(C, Avoid) of
        true -> DB;
        false ->
            maybe_add_child_port(C, Paren, DB, GL)
    end.

%% Check if it is a port, then it is added
maybe_add_child_port(C, Paren, DB, GL) ->
    if
        is_port(C) ->
            add_prim(C, Paren, DB);
        true ->
            maybe_add_child_sasl(C, Paren, DB, GL)
    end.

%% Use SASL stuff if present
maybe_add_child_sasl(C, Paren, DB, GL) ->
    case check_sasl_ancestor(Paren, C) of
        yes ->					% Primary
            add_prim(C, Paren, DB);
        no ->					% Secondary
            add_sec(C, Paren, DB);
        dont_know ->
            maybe_add_child_gl(C, Paren, DB, GL)
    end.

%% Check group leader
maybe_add_child_gl(C, Paren, DB, GL) ->
    case cmp_groupl(GL, groupl(C)) of
        true -> maybe_add_child_sec(C, Paren, DB);
        _ -> DB
    end.

%% Check if the link should be a secondary one. Note that this part is
%% pretty much a guess.
maybe_add_child_sec(C, Paren, DB) ->
    case is_in_queue(DB, C) of
        true ->					% Yes, secondary
            add_sec(C, Paren, DB);
        _ ->					% Primary link
            add_prim(C, Paren, DB)
    end.

check_sasl_ancestor(Paren, C) ->
    case lists:keysearch('$ancestors', 1,
                         element(2,process_info(C, dictionary))) of
        {value, {_, L}} when is_list(L) ->
            H = if
                    is_atom(hd(L)) -> whereis(hd(L));
                    true -> hd(L)
                end,
            if
                H == Paren -> yes;
                true -> no
            end;
        _ -> dont_know
    end.


%----------------------------------------------------------------------
%%---------------------------------------------------------------------
%% Primitives for the database DB of all links, processes and the
%% queue of not visited yet processes.

add_link(C, Paren, L) -> ets:insert(L, {Paren, C}).

new_db(Mode, Pid) ->
    P  = ets:new(processes, [set, public]),
    L1 = ets:new(links, [bag, public]),
    L2 = ets:new(extralinks, [bag, public]),
    Q = if
            Mode =:= sup -> queue:in({master, Pid}, queue:new());
            true -> queue:in(Pid, queue:new())
        end,
    #db{q=Q, p=P, links=L1, links2=L2}.

get_next(DB) ->
    {X, Q} = queue:out(DB#db.q),
    {X, DB#db{q=Q}}.
add_proc(DB, P) ->
    ets:insert(DB#db.p, {P}).
add_prim(C, Paren, DB) ->
    add_link(get_pid(C), Paren, DB#db.links),
    DB#db{q=queue:in(C, DB#db.q)}.
add_foreign(C, Paren, DB) ->
    add_link(C, Paren, DB#db.links2),
    DB#db{q=queue:in(C, DB#db.q)}.
add_sec(C, Paren, DB) ->
    add_link(C, Paren, DB#db.links2),
    DB.

is_proc(#db{p=Tab}, P) ->
    ets:member(Tab, P).

is_in_queue(#db{q=Q}, P) ->
    queue:member(P, Q).

%% Group leader handling. No processes or Links to processes must be
%% added when group leaders differ. Note that catch all is needed
%% because net_sup is undefined when not networked but still present
%% in the kernel_sup child list. Blahh, didn't like that.
groupl(P) ->
    case process_info(P, group_leader) of
        {group_leader, GL} -> GL;
        _Other -> nil
    end.

cmp_groupl(_GL1, nil) -> true;
cmp_groupl(GL1, GL1) -> true;
cmp_groupl(_, _) -> false.


%% Do some intelligent guessing as to cut in the tree
find_avoid() ->
    lists:foldr(fun(X, Accu) ->
                       case whereis(X) of
                           P when is_pid(P) ->
                               [P|Accu];
                           _ -> Accu end end,
                [undefined],
                ?IGNORE_APPS).

pid_info({P}) ->
    L =
        case process_info(P, registered_name) of
            {registered_name, Name} -> [{name, Name}];
            _ -> []
        end,
    L1 =
        case process_info(P, current_function) of
            {current_function, CMfa} -> [{cf, tuple_to_list(CMfa)}];
            _ -> []
        end,
    L2 =
        case process_info(P, dictionary) of
            {dictionary, Dict} when is_list(Dict) ->
                case lists:keyfind('$initial_call', 1, Dict) of
                    {'$initial_call', IcMfa} -> [{ic, tuple_to_list(IcMfa)}];
                    _ -> []
                end;
            _ -> []
        end,
    L3 =
        case process_info(P, message_queue_len) of
            {message_queue_len, QLen} -> [{q, QLen}];
            _ -> []
        end,
    {true, {list_to_binary(pid_to_list(P)), L ++ L1 ++ L2 ++ L3}};
pid_info(_) -> false.
