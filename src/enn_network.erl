-module(enn_network).

%% API
-export([new/1, create/1, load/1, load/2, backup/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Spec) ->
    Genom = create(Spec),
    load(Genom).

create(#{ layers := Layers }) ->
    create_layers(Layers, 1, [], []).

load(Genom) -> load(Genom, #{}).
load(Genom, Phenom) ->
    link_nodes(lists:foldr(fun load_layer/2, Phenom, Genom)).

backup(Phenom) ->
    backup_layer(1, Phenom, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_layers([#{ count := Cs }=L|Ls], LId, Sources, Acc)
  when is_list(Cs) ->
    create_layers(
      [maps:merge(L, #{ count => C })
       || C <- Cs] ++ Ls,
      LId, Sources, Acc);
create_layers([L|Ls], LId, Sources, Acc) ->
    create_layers(Ls, LId + 1, [LId], [create_layer(L, LId, 1, Sources, [])|Acc]);
create_layers([], _, _, Acc) -> lists:reverse(Acc).

create_layer(#{ count := Count }, _, Id, _, Acc) when Id > Count ->
    lists:reverse(Acc);
create_layer(#{ type := Type }=L, LId, Id, Sources, Acc) ->
    create_layer(
      L, LId, Id + 1, Sources,
      [#{ id => {LId, Id}, type => Type, sources => Sources }|Acc]).


load_layer([], Phenom) -> Phenom;
load_layer(L, Phenom) ->
    {Phenom1, LayerId, Nodes} =
        lists:foldl(
          fun (#{ id := Id = {Layer, _}, type := Type }=N, {P, LId, Ns})
              when LId == undefined; LId == Layer ->
                  Pid = enn_node:new(Type),
                  {maps:put(Pid, N, maps:put(Id, Pid, P)), Layer, [Pid|Ns]}
          end, {Phenom, undefined, []}, L),
    maps:put(LayerId, Nodes, Phenom1).

link_nodes(Phenom) ->
    maps:map(
      fun (Pid, #{ sources := Sources } = Node)
            when is_pid(Pid) ->
              maps:put(sources, link_node(Pid, Phenom, Sources), Node);
          (_, Node) -> Node
      end, Phenom).

link_node(Pid, Phenom, Sources) ->
    Pids = lists:flatten(lists:foldl(link_node(Phenom), [], Sources)),
    [begin
         ok = enn_node:add_source(Pid, Source, random),
         Source
     end || Source <- Pids].

link_node(Phenom) ->
    fun (Id, Sources) ->
            [maps:get(Id, Phenom)|Sources]
    end.

backup_layer(LId, Phenom, Acc) ->
    case maps:find(LId, Phenom) of
        error -> lists:reverse(Acc);
        {ok, L} -> backup_layer(
                     LId + 1, Phenom,
                     [backup_nodes(L, Phenom, [])|Acc])
    end.

backup_nodes([], _, Acc) -> lists:reverse(Acc);
backup_nodes([N|Ns], Phenom, Acc) ->
    G = maps:merge(maps:get(N, Phenom), enn_node:backup(N)),
    backup_nodes(Ns, Phenom, [G|Acc]).

%% backup(Pid, Genom, Phenom) ->
    
%%     {Type, backup_genom(Type, Pid, Genom, NN)}.

%% backup_genom(node, Pid, Genom, NN) ->
%%     #{ inputs := Srcs } = G = enn_node:backup(Pid),
%%     Inputs = maps:fold(
%%                fun (P, {W, _}, M) ->
%%                        maps:put(
%%                          phenom_id(maps:get(P, NN)),
%%                          W, M)
%%                end, #{}, Srcs),
%%     maps:merge(Genom, G#{ inputs := Inputs });
%% backup_genom(_, _, Genom, _) -> Genom.


-ifdef(TEST).

simple_genom() ->
    SensorType = {sensor, [rng]},
    NeuronType = {neuron, [#{ af => hardlim }]},
    ActuatorType = {actuator, [stdout]},
    create(#{ layers => [#{ count => 1, type => SensorType },
                         #{ count => 2, type => NeuronType },
                         #{ count => 1, type => ActuatorType }
                        ]
            }).

create_test() ->
    Genotype = simple_genom(),
    SensorType = {sensor, [rng]},
    NeuronType = {neuron, [#{ af => hardlim }]},
    ActuatorType = {actuator, [stdout]},
    ?assertEqual([[#{ id => {1, 1}, type => SensorType, sources => [] }],
                  [#{ id => {2, 1}, type => NeuronType, sources => [1] },
                   #{ id => {2, 2}, type => NeuronType, sources => [1] }],
                  [#{ id => {3, 1}, type => ActuatorType, sources => [2] }]
                 ], Genotype).

load_test() ->
    Genotype = simple_genom(),
    Phenotype = load(Genotype),
    SensorType = {sensor, [rng]},
    NeuronType = {neuron, [#{ af => hardlim }]},
    ActuatorType = {actuator, [stdout]},
    [begin
         Pid = maps:get(Id, Phenotype),
         Node1 = maps:put(sources, maps:get(Layer - 1, Phenotype, []), Node),
         ?assertEqual(Node1, maps:get(Pid, Phenotype))
     end || #{ id := Id = {Layer, _} } = Node <-
                [#{ id => {1, 1}, type => SensorType },
                 #{ id => {2, 1}, type => NeuronType },
                 #{ id => {2, 2}, type => NeuronType },
                 #{ id => {3, 1}, type => ActuatorType }
                ]
    ],
    %% give a few ticks to the actuator to print it's stuff..
    receive after 10 -> ok end.


-endif.
