-module(enn_network).

%% API
-export([new/2, load/1]).

%% Internal
-export([run/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(node, {
          id :: term(),
          pid :: pid(),
          activate,
          links = [] :: list({term(), number()})
         }).

-record(network, {
          time = 0,
          inputs = [],
          nodes = []
         }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(InputNodes, NodeDefs) ->
    Network = create(InputNodes, NodeDefs),
    spawn_link(?MODULE, run, [Network]).

load(Filename) ->
    {ok, Network} = file:script(Filename),
    load_network(Network).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Network) ->
    receive
        {input, Levels} ->
            Network1 = trigger(Levels, Network),
            run(Network1);
        _Other ->
            run(Network)
    end.

create(InputNodes, NodeDefs) ->
    Nodes = create_nodes(NodeDefs),
    #network{
       inputs = [resolve_node_id(Id, Nodes) || Id <- InputNodes],
       nodes = Nodes
      }.

trigger(Levels, #network{ inputs = Pids, time = Time } = Network) ->
    [Pid ! {activity, {Time, Level}} || {Pid, Level} <- lists:zip(Pids, Levels)],
    Network#network{ time = Time + 1 }.

create_nodes(Nodes) ->
    create_nodes(Nodes, []).

create_nodes([], Acc) -> Acc;
create_nodes([Node|Nodes], Acc) ->
    Links = [{resolve_node_id(Id, Acc), W} || {Id, W} <- Node#node.links],
    Node1 = Node#node{ pid = enn_node:new(Node#node.activate, Links) },
    create_nodes(Nodes, [Node1|Acc]).

resolve_node_id(Pid, _) when is_pid(Pid) -> Pid;
resolve_node_id(Id, [#node{ id = Id, pid = Pid }|_]) -> Pid;
resolve_node_id(Id, [_|Nodes]) -> resolve_node_id(Id, Nodes);
resolve_node_id(Id, []) -> throw({unknown_node_id, Id}).


load_network({Name, Inputs, Nodes}) ->
    true = register(Name, new(Inputs, [define_node(Node) || Node <- Nodes])).

define_node(Props) ->
    #node{
       id = proplists:get_value(id, Props),
       activate = case proplists:get_value(f, Props, purelin) of
                      F when is_function(F, 1) -> F;
                      A when is_atom(A) -> fun enn_f:A/1;
                      {M, F} -> fun M:F/1
                  end,
       links = proplists:get_value(links, Props, [{self(), 1}])
      }.


-ifdef(TEST).

test_activity(Activity) ->
    receive
        {activity, Activity} -> ok;
        Other -> throw({activity, {expected, Activity}, {actual, Other}})
    after
        10 -> none
    end.


simple_network_test() ->
    F = fun (I) ->
                if I >= 1 -> 1;
                   true -> 0
                end
        end,
    %% 1 input node, 1 output node
    NN = create(
           [input],
           [#node{ id=output, activate=F, links=[{self(), 2.2}] },
            #node{ id=input, activate=F, links=[{output, 5}] }
           ]
          ),
    trigger([2.3], NN),
    ?assertMatch(ok, test_activity({2, 2.2})).


-endif.
