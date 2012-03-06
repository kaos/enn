%%%-------------------------------------------------------------------
%%% File    : enn_network.erl
%%% Author  : Andreas Stenius <git@astekk.se>
%%% Description : 
%%%
%%% Created :  6 Mar 2012 by Andreas Stenius <git@astekk.se>
%%%-------------------------------------------------------------------
-module(enn_network).

-behaviour(enn_block).

%% API
-export([
         new/1,
         input/2,
         step/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("enn.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: new([Block :: enn_block()]) -> term()
%% Description: Create a newtwork of neural building blocks.
%%--------------------------------------------------------------------
new(Args) ->
    enn:new(Args).

input(Network, Input) ->
    lists:foldl(fun enn:input/2, Input, Network).

step(Network) ->
    lists:foldl(
      fun
          (Layer, undefined) -> enn:step(Layer);
          (Layer, Input) -> enn:input(Layer, Input)
      end,
      undefined,
      Network).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


-ifdef(TEST).

simple_two_layer_network_test() ->
    N = new([
             %% First layer (Input layer)
             {enn_neuron, [#neuron{w=[1.5], b=-0.5, f=purelin}]},
             
             %% Second layer (Output layer)
             {enn_neuron, [#neuron{w=[1.5], b=0.5, f=purelin}]}
            ]),
    ?assertEqual([6.5], input(N, [3])).

two_layers_three_inputs_on_two_neurons_one_output_test() -> 
    N = new([
             %% First layer (Input layer)
             {enn_neuron, [
                           #neuron{w=[-1.5, 1, 1.5], b=0, f=purelin},
                           #neuron{w=[1, 2, -3], b=2, f=purelin}
                          ]},
             
             %% Second layer (Output layer)
             {enn_neuron, [#neuron{w=[1.5, -1.5], b=-5, f=purelin}]}
            ]),
    ?assertEqual([22.75], input(N, [2, -3, 5])).

simple_recurrent_network_test() ->
    N = new([
             %% First (and only) layer
             {enn_delay_block, 
              [
               %% update fun for D block
               {input, fun enn:input/2},
               %% reference to pass to update fun
               {ref, enn:new(enn_neuron, [#neuron{w=[1], b=1, f=purelin}])}
              ]}
            ]),
    ?assertEqual([5], input(N, [5])),
    ?assertEqual([6], step(N)),
    ?assertEqual([7], step(N)).

apples_and_oranges_hamming_test() ->
    %% using the enn api to get it tested (and as a working example/how to)
    %% this test is based on the hamming network in chapter 3 of "Neural Network Design"
    %% we use three inputs to represent shape, texture and weight
    %% and two outputs to classify oranges and apples.
    N = enn:new({enn_network, 
                 [
                  %% the feedforward layer
                  {enn_neuron, 
                   [
                    %% the prototype orange
                    #neuron{w=[1, -1, -1], b=3, f=purelin},
                    %% and the prototype apple
                    #neuron{w=[1, 1, -1], b=3, f=purelin}
                   ]},
                  %% the recurrent layer
                  {enn_delay_block,
                   [
                    {input, fun enn:input/2},
                    {ref, enn:new(
                            {enn_neuron,
                             [
                              #neuron{w=[1, -0.5], b=0, f=poslin},
                              #neuron{w=[-0.5, 1], b=0, f=poslin}
                             ]}
                           )}
                   ]}
                 ]}
               ),

    %% test with the oblong orange as input
    ?assertEqual([4, 2], enn:input(N, [-1, -1, -1])),
    %% converges to
    ?assertEqual([3.0, 0], enn:step(N)),
    %% make sure it is stable
    ?assertEqual([3.0, 0], enn:step(N)),
    
    %% test with an apple as input
    ?assertEqual([4, 6], enn:input(N, [1, 1, -1])),
    %% watch it converge...
    ?assertEqual([1.0, 4.0], enn:step(N)),
    ?assertEqual([0, 3.5], enn:step(N)),
    ?assertEqual([0, 3.5], enn:step(N)).

                               
-endif.
