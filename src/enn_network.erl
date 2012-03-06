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
    [0 || _ <- Network].

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


-endif.
