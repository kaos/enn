%%% File    : enn_block.erl
%%% Author  : Andreas Stenius <git@astekk.se>
%%% Description : 
%%% Created :  6 Mar 2012 by Andreas Stenius <git@astekk.se>

-module(enn_block).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {new, 1},
     {input, 2},
     {step, 1}
    ].
