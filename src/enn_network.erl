-module(enn_network).

%% API
-export([create/1]). %new/2, load/1, save/2]).

%% Internal
%% -export([run/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% -record(node, {
%%           id :: term(),
%%           pid :: pid(),
%%           activator = purelin,
%%           threshold = random,
%%           links = [] :: list({term(), number()})
%%          }).

%% -record(network, {
%%           inputs = [],
%%           nodes = []
%%          }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Opts) ->
    Network = [create_layer(L) || L <- maps:get(layers, Opts)],
    InputLayer = hd(Network),
    OutputLayer = hd(lists:reverse(Network)),
    [link_sensor(S, InputLayer) || S <- maps:get(sensors, Opts, [])],
    [link_actuator(A, OutputLayer) || A <- maps:get(actuators, Opts, [])],
    {ok, Network}.

%% new(InputNodes, NodeDefs) ->
%%     Network = create(InputNodes, NodeDefs),
%%     spawn_link(?MODULE, run, [Network]).

%% load(Filename) ->
%%     {ok, Network} = file:script(Filename),
%%     load_network(Network).

%% save(Filename, Network) ->
%%     Network ! {save, Filename},
%%     ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_layer(Layer) when is_list(Layer), length(Layer) > 0 ->
    maps:from_list([create_node(Node) || Node <- Layer]).

create_node(#{ id := Id, af := AF } = Node) ->
    {Id, enn_node:new(Id, AF, maps:get(thld, Node, 0.0))}.

link_sensor(S, Layer) ->
    [begin
         ok = enn_node:add_source(N, S, random)
     end || N <- maps:values(Layer)].

link_actuator(A, Layer) ->
    [begin
         ok = enn_node:add_target(N, A),
         A ! {N, source}
     end || N <- maps:values(Layer)].

%% run(Network) ->
%%     receive
%%         {input, Levels} ->
%%             Network1 = trigger(Levels, Network),
%%             run(Network1);
%%         {save, Filename} ->
%%             ok = save_network(Filename, Network),
%%             run(Network);
%%         _Other ->
%%             run(Network)
%%     end.

%% create(InputNodes, NodeDefs) ->
%%     Nodes = create_nodes(NodeDefs),
%%     #network{
%%        inputs = [resolve_node_id(Id, Nodes) || Id <- InputNodes],
%%        nodes = Nodes
%%       }.

%% trigger(Levels, #network{ inputs = Pids } = Network) ->
%%     [Pid ! {activity, {self(), 0, Level}} || {Pid, Level} <- lists:zip(Pids, Levels)],
%%     Network.

%% create_nodes(Nodes) ->
%%     create_nodes(Nodes, []).

%% create_nodes([], Acc) -> lists:reverse(Acc);
%% create_nodes([Node|Nodes], Acc) ->
%%     Links = [{resolve_node_id(Id, Acc), W} || {Id, W} <- Node#node.links],
%%     Node1 = Node#node{ pid = enn_node:new(Node#node.activator, Node#node.threshold, Links) },
%%     create_nodes(Nodes, [Node1|Acc]).

%% resolve_node_id(Name, []) -> Name;
%% resolve_node_id(Pid, _) when is_pid(Pid) -> Pid;
%% resolve_node_id(Id, [#node{ id = Id, pid = Pid }|_]) -> Pid;
%% resolve_node_id(Id, [_|Nodes]) -> resolve_node_id(Id, Nodes).

%% resolve_node_pid(Id, _) when is_atom(Id) -> Id;
%% resolve_node_pid(Pid, [#node{ pid = Pid, id = Id }|_]) -> Id;
%% resolve_node_pid(Pid, [_|Nodes]) -> resolve_node_pid(Pid, Nodes);
%% resolve_node_pid(_Pid, []) -> [].

%% load_network({Name, Inputs, Nodes}) ->
%%     true = register(Name, new(Inputs, [define_node(Node) || Node <- Nodes])).

%% define_node(Props) ->
%%     #node{
%%        id = proplists:get_value(id, Props),
%%        activator = proplists:get_value(f, Props, purelin),
%%        links = proplists:get_value(links, Props, [{self(), 1}])
%%       }.

%% save_network(Filename, Network) ->
%%     {ok, Contents} = enn_network_dtl:render(
%%                        [{network, filename:basename(Filename)},
%%                         {inputs, [resolve_node_pid(Pid, Network#network.nodes) || Pid <- Network#network.inputs]},
%%                         {nodes, [[{id, Node#node.id},
%%                                   {activator, Node#node.activator},
%%                                   {threshold, Node#node.threshold},
%%                                   {links, Node#node.links}] || Node <- Network#network.nodes]}],
%%                        [{auto_escape, false}]),
%%     file:write_file(Filename, Contents).


-ifdef(TEST).

create_test() ->
    {ok, Network} =
        create(
          #{
             sensors => [self()],
             actuators => [self()],
             layers => [[#{ id => a, af => hardlim }]]
           }),
    ?assertMatch([#{ a := _ }], Network),
    receive {_, target} -> ok after 1 -> throw(err) end,
    receive {_, source} -> ok after 1 -> throw(err) end.

%% first_test_no() ->
%%     %%{Xs, Ys} = create(#network{ inputs = 2, outputs = 1, layers = []}),
%%     {[N] = Xs, [N] = Ys} = create([a], [a], [{a, hardlim, 0.0, []}]),
%%     connect([{self(), 1.0}], Xs),
%%     [enn_node:add_target(Y, self()) || Y <- Ys],
%%     [enn_node:activate(X, 1.0) || X <- Xs],
%%     receive
%%         {N, activity, 1.0} -> ok
%%     after 1 ->
%%             throw(no_activity)
%%     end.

%% test_activity(Activity) ->
%%     receive
%%         {activity, Activity} -> ok;
%%         Other -> throw({activity, {expected, Activity}, {actual, Other}})
%%     after
%%         10 -> none
%%     end.


%% simple_network_test() ->
%%     %% 1 input node, 1 output node
%%     NN = create(
%%            [input],
%%            [#node{ id=output, activator=hardlim, threshold=0, links=[{self, 2}] },
%%             #node{ id=input, activator=hardlim, threshold=0, links=[{output, 1}] }
%%            ]
%%           ),
%%     trigger([0.3], NN),
%%     ?assertMatch(ok, test_activity(
%%                        {resolve_node_id(output, NN#network.nodes), 2, 2.0}
%%                       )).

%% or_gate_test() ->
%%     NN = create(
%%            [a, b],
%%            [#node{ id=output, activator=hardlim, links=[{self, random}] },
%%             #node{ id=a, activator=hardlim, links=[{output, random}] },
%%             #node{ id=b, activator=hardlim, links=[{output, random}] }
%%            ]
%%           ),
%%     OutPid = resolve_node_id(output, NN#network.nodes),
%%     [begin
%%          trigger(Input, NN),
%%          ?debugFmt("~p~n", [{Result, Input, Output}]),
%%          ?assertMatch(Result, test_activity({OutPid, 2, Output}))
%%      end || {Result, Input, Output} <- [{none, [0,0], 0.0},
%%                                         {ok,   [1,1], 1.0},
%%                                         {ok,   [1,0], 1.0},
%%                                         {ok,   [0,1], 1.0}
%%                                        ]].


-endif.
