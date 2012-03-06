%%%-------------------------------------------------------------------
%%% File    : enn_f.erl
%%% Author  : Andreas Stenius <git@astekk.se>
%%% Description : 
%%%
%%% Created :  5 Mar 2012 by Andreas Stenius <git@astekk.se>
%%%-------------------------------------------------------------------
-module(enn_f).

%% API
-export([
%%          e/1,

         purelin/1,
         poslin/1,
         hardlim/1,
         hardlims/1,
         satlin/1,
         satlins/1,
         logsig/1,
         tansig/1

        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(e, 2.718281828459045).

%%====================================================================
%% API
%%====================================================================

%% e(N) when N >= 0 ->
%%     e(2, 1, 2, N).
%% e(Acc, _, _, 0) ->
%%     Acc;
%% e(Acc, Div0, Mul, Count) ->
%%     Div1 = Div0 * Mul,
%%     e(Acc+(1/Div1), Div1, Mul + 1, Count - 1).

%%--------------------------------------------------------------------
%% Function: purelin(N) -> N
%% Description: a pure linear transfer function
%%--------------------------------------------------------------------
purelin(N) -> N.

%%--------------------------------------------------------------------
%% Function: poslin(N) -> N
%% Description: a positive pure linear transfer function
%%--------------------------------------------------------------------
poslin(N) when N =< 0 -> 0;
poslin(N) -> N.

%%--------------------------------------------------------------------
%% Function: hardlim(N) -> A
%% Description: Hard limit. Output is either 0 or 1.
%%--------------------------------------------------------------------
hardlim(N) when N < 0 -> 0;
hardlim(N) when N >= 0 -> 1.
    
%%--------------------------------------------------------------------
%% Function: hardlims(N) -> A
%% Description: Symmetrical hard limit. Output is either -1 or 1.
%%--------------------------------------------------------------------
hardlims(N) when N < 0 -> -1;
hardlims(N) when N >= 0 -> 1.

%%--------------------------------------------------------------------
%% Function: satlin(N) -> A
%% Description: Saturating linear. Output is between 0 and 1.
%%--------------------------------------------------------------------
satlin(N) when N =< 0 -> 0;
satlin(N) when N >= 1 -> 1;
satlin(N) when is_number(N) -> N.
    
%%--------------------------------------------------------------------
%% Function: satlins(N) -> A
%% Description: Symmetric saturating linear. Output is between -1 and 1.
%%--------------------------------------------------------------------
satlins(N) when N =< -1 -> -1;
satlins(N) when N >= 1 -> 1;
satlins(N) when is_number(N) -> N.

%%--------------------------------------------------------------------
%% Function: logsig(N) -> A
%% Description: Log-Sigmoid. Output is between 0 and 1.
%%--------------------------------------------------------------------
logsig(N) when is_number(N) -> 1/(1+math:pow(?e, -N)).

%%--------------------------------------------------------------------
%% Function: tansig(N) -> A
%% Description: Hyperbolic tangent sigmoid. Output is between -1 and 1.
%%--------------------------------------------------------------------
tansig(N) when is_number(N) -> 
    En = math:pow(?e, N),
    E_n = math:pow(?e, -N),
    (En - E_n) / (En + E_n).


%%====================================================================
%% Internal functions
%%====================================================================



-ifdef(TEST).

purelin_test() ->
    42 = purelin(42).

poslin_test() ->
    0 = poslin(-0.4),
    0.4 = poslin(0.4),
    1.4 = poslin(1.4).

hardlim_test() ->
    0 = hardlim(-5),
    0 = hardlim(-1),
    0 = hardlim(-0.0001),
    1 = hardlim(0),
    1 = hardlim(0.0001),
    1 = hardlim(1),
    1 = hardlim(12).

hardlims_test() ->
    -1 = hardlims(-5.5),
    -1 = hardlims(-0.1),
    1 = hardlims(0.0),
    1 = hardlims(2.2).

satlin_test() ->
    0 = satlin(-1.5),
    0 = satlin(0),
    0.123 = satlin(0.123),
    0.987 = satlin(0.987),
    1 = satlin(1.0),
    1 = satlin(2.43),
    1 = satlin(5).

satlins_test() ->
    -1 = satlins(-2),
    -1 = satlins(-1.0),
    -0.5 = satlins(-0.5),
    0.5 = satlins(0.5),
    1 = satlins(1),
    1 = satlins(123.23).

logsig_test() ->
    ?assert(0.0001 > logsig(-10)),
    ?assert(0.5 == logsig(0)),
    ?assert(0.9999 < logsig(10)).


-endif.
