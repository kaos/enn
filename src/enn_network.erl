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
    Network = [create_layer(L, Template) || L <- maps:get(layers, Opts)],
    InputLayer = hd(Network),
    OutputLayer = hd(lists:reverse(Network)),
    Sensors = [link_sensor(S, InputLayer) || S <- maps:get(sensors, Opts, [])],
    Actuators = [link_actuator(A, OutputLayer) || A <- maps:get(actuators, Opts, [])],
    {ok, #{ network => Network, sensors => Sensors, actuators => Actuators }}.

backup(_Network) ->
    {ok, nyi}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_layer(Layer, Template) when is_list(Layer), length(Layer) > 0 ->
    maps:from_list([create_node(maps:merge(Template, Node)) || Node <- Layer]);
create_layer(Density, Template) when is_integer(Density), Density > 0 ->
    create_layer([maps:merge(Template, #{ id => Id }) || Id <- lists:seq(1, Density)], Template).

create_node(#{ id := Id, af := AF } = Node) ->
    {Id, Node#{ pid => enn_node:new(Id, AF, maps:get(thld, Node, 0.0)) }}.

link_sensor(S, Layer) when is_pid(S) ->
    [begin
         ok = enn_node:add_source(N, S, random)
     end || #{ pid := N } <- maps:values(Layer)],
    S;
link_sensor({M, F, A}, Layer) ->
    link_sensor(apply(M, F, A), Layer).

link_actuator(A, Layer) when is_pid(A) ->
    [begin
         ok = enn_node:add_target(N, A),
         A ! {N, source}
     end || #{ pid := N } <- maps:values(Layer)],
    A;
link_actuator({M, F, A}, Layer) ->
    link_actuator(apply(M, F, A), Layer).


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
