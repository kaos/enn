-module(enn_network).

%% API
-export([create/1, backup/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Opts) ->
    Template = maps:get(node_template, Opts, #{}),
    {Network, _} = lists:mapfoldl(
                     fun (L, LId) ->
                             {create_layer(L, Template#{ layer => LId }), LId + 1}
                     end, 1, maps:get(layers, Opts)),
    InputLayer = hd(Network),
    OutputLayer = hd(lists:reverse(Network)),
    Sensors = [link_sensor(S, InputLayer) || S <- maps:get(sensors, Opts, [])],
    Actuators = [link_actuator(A, OutputLayer) || A <- maps:get(actuators, Opts, [])],
    {ok, lists:foldl(fun maps:merge/2, #{}, Sensors ++ Actuators ++ Network) }.

backup(NN) ->
    {ok, [backup(Pid, Phenom, NN) || {Pid, Phenom} <- maps:to_list(NN)]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_layer(Layer, Template) when is_list(Layer), length(Layer) > 0 ->
    maps:from_list([create_node(maps:merge(Template, Node)) || Node <- Layer]);
create_layer(Density, Template) when is_integer(Density), Density > 0 ->
    create_layer([maps:merge(Template, #{ id => Id }) || Id <- lists:seq(1, Density)], Template).

create_node(#{ af := AF } = Node) ->
    Pid = enn_node:new(AF, maps:get(thld, Node, 0.0)),
    {Pid, fun (pid) -> Pid;
              (genom) -> {node, Node}
          end}.

link_sensor({M, F, A}=Genom, Layer) ->
    Pid = apply(M, F, A),
    link_sensor(
      fun (pid) -> Pid;
          (genom) -> {sensor, Genom}
      end, Layer);
link_sensor(Sensor, Layer)  ->
    Pid = phenom_pid(Sensor),
    [begin
         ok = enn_node:add_source(phenom_pid(Node), Pid, random)
     end || Node <- maps:keys(Layer)],
    maps:put(Pid, Sensor, #{}).

link_actuator({M, F, A}=Genom, Layer) ->
    Pid = apply(M, F, A),
    link_actuator(
      fun (pid) -> Pid;
          (genom) -> {actuator, Genom}
      end, Layer);
link_actuator(Actuator, Layer) ->
    A = phenom_pid(Actuator),
    [begin
         N = phenom_pid(Node),
         ok = enn_node:add_target(N, A),
         A ! {N, source}
     end || Node <- maps:keys(Layer)],
    maps:put(A, Actuator, #{}).

phenom_pid(Pid) when is_pid(Pid) -> Pid;
phenom_pid(#{ pid := Pid }) when is_pid(Pid) -> Pid;
phenom_pid(Phenom) when is_function(Phenom, 1) ->
    phenom_pid(Phenom(pid)).

phenom_id(Phenom) when is_function(Phenom, 1) ->
    case Phenom(genom) of
        {sensor, _} -> sensor;
        {actuator, _} -> actuator;
        {node, #{ layer := LId, id := Id }} ->
                {node, {LId, Id}}
        end.

backup(Pid, Phenom, NN) ->
    {Type, Genom} = Phenom(genom),
    {Type, backup_genom(Type, Pid, Genom, NN)}.

backup_genom(node, Pid, Genom, NN) ->
    #{ inputs := Srcs } = G = enn_node:backup(Pid),
    Inputs = maps:fold(
               fun (P, {W, _}, M) ->
                       maps:put(
                         phenom_id(maps:get(P, NN)),
                         W, M)
               end, #{}, Srcs),
    maps:merge(Genom, G#{ inputs := Inputs });
backup_genom(_, _, Genom, _) -> Genom.


-ifdef(TEST).

create_test() ->
    {ok, Network} =
        create(
          #{
             sensors => [self()],
             actuators => [self()],
             layers => [[#{ id => a, af => hardlim }]]
           }),
    ?assertMatch(#{
                    network := [#{ a := _ }], 
                    sensors := [_],
                    actuators := [_]
                  }, Network),
    receive {_, target} -> ok after 1 -> throw(err) end,
    receive {_, source} -> ok after 1 -> throw(err) end.



-endif.
