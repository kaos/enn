%%%-------------------------------------------------------------------
%%% File    : enn_perceptron.erl
%%% Author  : Andreas Stenius <git@astekk.se>
%%% Description : 
%%%
%%% Created :  5 Mar 2012 by Andreas Stenius <git@astekk.se>
%%%-------------------------------------------------------------------
-module(enn_perceptron).

-behaviour(enn_block).

%% API
-export([
         new/1,
         input/2,
         step/1,

         learn/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("enn.hrl").


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: new([#neuron{}]) -> term()
%% Description: Create a neuron layer
%%--------------------------------------------------------------------
new(Args) ->
    [
     N#neuron{
       w = [float(W) || W <- N#neuron.w], 
       b = float(N#neuron.b)
      } 
     || N <- Args, 
        is_record(N, neuron)
           ].

input(Layer, Input) ->
    [get_output(Neuron, Input) || Neuron <- Layer].

step(_Layer) ->
    undefined.

%% Perceptron learning rule
learn(Layer, DataSet) -> learn(Layer, DataSet, []).

learn(Layer, [], E) -> {Layer, lists:reverse(E)};
learn(Layer, [{P, T}|DataSet], EIn) ->
    E = lists:zipwith(fun(Ts, As) -> Ts - As end, T, input(Layer, P)),
    learn(
      train(Layer, P, E),
      DataSet,
      [E|EIn]
     ).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_output(#neuron{ w=W, b=B, f=F }, Input) ->
    transfer_net_input(F, B + lists:sum(lists:zipwith(fun(Wr, Pr) -> Wr * Pr end, W, Input))).

transfer_net_input(F, N) when is_function(F) ->
    F(N);
transfer_net_input(F, N) when is_atom(F) ->
    enn_f:F(N);
transfer_net_input({M, F}, N) ->
    M:F(N).

train(Layer, P, E) ->
    %% W' = W + E*P ; W(RxS) E(Sx1) P(Rx1) ; R number of inputs, S number of neurons (outputs)
    %% B' = B + E ; B(Sx1)
    [ N#neuron{ w=lists:zipwith(fun(W, Pr) -> W + Es * Pr end, W0, P), b=B0 + Es }
      || {#neuron{ w=W0, b=B0 }=N, Es} <- lists:zip(Layer, E)
            ].


-ifdef(TEST).

single_neuron_bias_test() ->
    N = new([#neuron{ w=[0], b=321, f=purelin }]),
    ?assertEqual([321.0], input(N, [123])).

single_neuron_weight_test() ->
    N = new([#neuron{ w=[1], b=0, f=fun enn_f:purelin/1 }]),
    ?assertEqual([123.0], input(N, [123])).
    
single_neuron_multi_input_test() ->
    N = new([#neuron{ w=[1, 2, 3], b=0, f={enn_f,purelin} }]),
    ?assertEqual([28.0], input(N, [6, 5, 4])).

single_neuron_multi_input_with_bias_test() ->
    N = new([#neuron{ w=[1.1, 2.2, 3.3], b=-10.5, f=purelin }]),
    ?assertEqual([0.5], input(N, [3, 2, 1])).

multiple_neuron_test() ->
    N = new([#neuron{w=[1, 2], b=3, f=purelin}, #neuron{w=[4, 5], b=6, f=purelin}]),
    ?assertEqual([26.0, float(7*4+8*5+6)], input(N, [7, 8])).

'P2.3_test'() ->
    New = fun(F) ->
                  new([#neuron{w=[3, 2], b=1.2, f=F}])
          end,
    [ ?assertEqual([A], input(New(F), [-5, 6])) 
      || {F, A} <- [
                    {purelin, -1.8},
                    {hardlims, -1.0},
                    {satlin, 0.0},
                    {tansig, -0.9468060128462682}
                   ] 
            ].

step_test() ->
    N = new([#neuron{}]),
    ?assertEqual(undefined, step(N)).

single_neuron_perceptron_learning_test() ->
    N = new( [#neuron{ w=[1, -0.8], b=0, f=hardlim }] ),
    DataSet = [
               {[1, 2], [1]}, 
               {[-1, 2], [0]}, 
               {[0, -1], [0]}
              ],
    F = fun(_, Nn, true) -> Nn;
           (F, Nn, false) ->
                {Next, E} = learn(Nn, DataSet),
                %% ?debugVal({Next, E}),
                F(F, Next, lists:all(fun(0.0) -> true; (_) -> false end, lists:flatten(E)))
        end,
    N1 = F(F, N, false),
    %% ?debugVal(N1),
    [
     %% doesn't seem like assertMatch works inside list comprehensions...
     ?assert(T == input(N1, P))
     || {P, T} <- DataSet
           ].


-endif.
