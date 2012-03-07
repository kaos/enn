%%%-------------------------------------------------------------------
%%% File    : enn.erl
%%% Author  : Andreas Stenius <git@astekk.se>
%%% Description : 
%%%
%%% Created :  6 Mar 2012 by Andreas Stenius <git@astekk.se>
%%%-------------------------------------------------------------------
-module(enn).

-behaviour(enn_block).

%% API
-export([
         new/1,
         new/2,

         input/2,
         step/1
        ]).

-include("enn.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: new([{Type :: enn_block_type(), Args}]) -> [term()]
%% Description: Create new enn building blocks.
%%--------------------------------------------------------------------
new(Layers) when is_list(Layers) ->
    [new(Layer) || Layer <- Layers];
new({Type, N}) ->
    new(Type, N).

%%--------------------------------------------------------------------
%% Function: new(Type :: enn_block_type(), N :: term()) -> term()
%% Description: Create new building block.
%%--------------------------------------------------------------------
new(Type, N) ->
    %% TODO: make this pluggable to allow dynamic adding of new types
    Mod = module_from_type(Type),
    %% TODO: check that Mod is supporting the enn_block behavior
    {Mod, Mod:new(N)}.

%%--------------------------------------------------------------------
%% Function: input(term(), [number()]) -> [number()]
%% Description: Give input to network, returns outputs at time t=0.
%%--------------------------------------------------------------------
input(Blocks, Input) when is_list(Blocks) ->
    [input(Block, Input) || Block <- Blocks];
input({Mod, N}, Input) ->
    Mod:input(N, Input).

%%--------------------------------------------------------------------
%% Function: step(term()) -> [number()]
%% Description: Get outputs at time t=t+1.
%%--------------------------------------------------------------------
step(Blocks) when is_list(Blocks) ->
    [step(Block) || Block <- Blocks];
step({Mod, N}) ->
    Mod:step(N).


%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------


%%====================================================================
%% Internal functions
%%====================================================================

%% Convert enn block type name to a module name implementing that type
module_from_type(perceptron) -> enn_perceptron;
module_from_type(network) -> enn_network;
module_from_type(delay) -> enn_delay_block;
module_from_type(TryAsModule) -> TryAsModule.
