%%%-------------------------------------------------------------------
%%% File    : enn_neuron.erl
%%% Author  : Andreas Stenius <git@astekk.se>
%%% Description : 
%%%
%%% Created :  5 Mar 2012 by Andreas Stenius <git@astekk.se>
%%%-------------------------------------------------------------------
-module(enn_neuron).

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
%% Function: new([#neuron{}]) -> term()
%% Description: Create a neuron layer
%%--------------------------------------------------------------------
new(Args) ->
    % todo: verify args
    Args.

input(Layer, Input) ->
    [get_output(Neuron, Input) || Neuron <- Layer].

step(_Layer) ->
    undefined.


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


-ifdef(TEST).

single_neuron_bias_test() ->
    N = new([#neuron{ w=[0], b=321, f=purelin }]),
    ?assertEqual([321], input(N, [123])).

single_neuron_weight_test() ->
    N = new([#neuron{ w=[1], b=0, f=fun enn_f:purelin/1 }]),
    ?assertEqual([123], input(N, [123])).
    
single_neuron_multi_input_test() ->
    N = new([#neuron{ w=[1, 2, 3], b=0, f={enn_f,purelin} }]),
    ?assertEqual([28], input(N, [6, 5, 4])).

single_neuron_multi_input_with_bias_test() ->
    N = new([#neuron{ w=[1.1, 2.2, 3.3], b=-10.5, f=purelin }]),
    ?assertEqual([0.5], input(N, [3, 2, 1])).

multiple_neuron_test() ->
    N = new([#neuron{w=[1, 2], b=3, f=purelin}, #neuron{w=[4, 5], b=6, f=purelin}]),
    ?assertEqual([26, 7*4+8*5+6], input(N, [7, 8])).

'P2.3_test'() ->
    New = fun(F) ->
                  new([#neuron{w=[3, 2], b=1.2, f=F}])
          end,
    [ ?assertEqual([A], input(New(F), [-5, 6])) 
      || {F, A} <- [
                    {purelin, -1.8},
                    {hardlims, -1},
                    {satlin, 0},
                    {tansig, -0.9468060128462682}
                   ] 
            ].

step_test() ->
    N = new([#neuron{}]),
    ?assertEqual(undefined, step(N)).

-endif.
