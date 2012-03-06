%%%-------------------------------------------------------------------
%%% File    : enn_neuron.erl
%%% Author  : Andreas Stenius <git@astekk.se>
%%% Description : 
%%%
%%% Created :  5 Mar 2012 by Andreas Stenius <git@astekk.se>
%%%-------------------------------------------------------------------
-module(enn_neuron).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         new/1,
         input/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("enn.hrl").

-record(state, {
          neurons=[]
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

new(Args) ->
    {ok, Ref} = start_link(Args),
    Ref.

input(Neuron, Input) ->
    gen_server:call(Neuron, {input, Input}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Args) ->
    {ok, #state{ 
       neurons=Args
      }
    }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({input, Input}, _From, #state{neurons=L}=State) ->
    Reply = [get_output(N, Input) || N <- L],
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    {ok, N} = start_link([#neuron{ w=[0], b=321, f=purelin }]),
    ?assertEqual([321], input(N, [123])).

single_neuron_weight_test() ->
    {ok, N} = start_link([#neuron{ w=[1], b=0, f=fun enn_f:purelin/1 }]),
    ?assertEqual([123], input(N, [123])).
    
single_neuron_multi_input_test() ->
    {ok, N} = start_link([#neuron{ w=[1, 2, 3], b=0, f={enn_f,purelin} }]),
    ?assertEqual([28], input(N, [6, 5, 4])).

single_neuron_multi_input_with_bias_test() ->
    {ok, N} = start_link([#neuron{ w=[1.1, 2.2, 3.3], b=-10.5, f=purelin }]),
    ?assertEqual([0.5], input(N, [3, 2, 1])).

multiple_neuron_test() ->
    {ok, N} = start_link([#neuron{w=[1, 2], b=3, f=purelin}, #neuron{w=[4, 5], b=6, f=purelin}]),
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

-endif.
