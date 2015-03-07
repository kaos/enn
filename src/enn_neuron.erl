-module(enn_neuron).

%%% API
-export([new/1]).

%%% Internal
-export([loop/1]).

-ifdef(TEST).
-export([dbg/0]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(neuron, {
          af = purelin :: fun((float()) -> float()), %% actuator function
          lvl = none :: none | float(), %% activation level
          thld = none :: none | float(),
          srcs = #{}, %% input sources
          tgts = [] :: [pid()] %% output targets
         }).


%%%----------------------------------------
%%% API
%%%----------------------------------------

new(Opts) when is_map(Opts) ->
    spawn_link(?MODULE, loop, [init(Opts)]).

%%%----------------------------------------
%%% Internal
%%%----------------------------------------

init(Opts) ->
    #{ af := AF, thld := Thld } =
        maps:merge(defaults(), Opts),
    #neuron{
       af = create_activator(AF),
       lvl = none,
       thld = enn_node:to_weight(Thld)
      }.

defaults() ->
    D = #neuron{},
    #{ af => D#neuron.af, 
       thld => D#neuron.thld
     }.

create_activator(Fun) when is_function(Fun, 1) -> Fun;
create_activator(Std) when is_atom(Std) -> fun enn_f:Std/1;
create_activator({M, F}) -> fun M:F/1;
create_activator({M, F, A}) -> create_activator(apply(M, F, A));
create_activator(L) when is_list(L) ->
    lists:foldl(
      fun (A, F) -> F(create_activator(A)) end,
      fun (F) -> F end, L).
    
loop(Neuron) ->
    receive
        {S, activity, A} when is_pid(S), is_float(A) ->
            loop(process(A, S, Neuron));
        {S, source, W} when is_pid(S) ->
            Srcs = maps:put(S, {enn_node:to_weight(W), 0.0}, Neuron#neuron.srcs),
            loop(Neuron#neuron{ srcs = Srcs });
        {T, target} when is_pid(T) ->
            loop(Neuron#neuron{ tgts = [T|Neuron#neuron.tgts] });
        {R, backup} ->
            R ! {self(), backup, backup(Neuron)},
            loop(Neuron)
    end.

process(A, S, #neuron{ lvl = L0, srcs = Ss } = Neuron) ->
    {W, A0} = maps:get(S, Ss),
    {L1, A1} =
        case A * W of
            Aa when L0 == none -> {Aa, Aa};
            A0 when is_number(L0) -> {L0, A0};
            Aa when is_number(L0) -> {L0 + Aa - A0, Aa}
    end,
    maybe_fire(L0, Neuron#neuron{ lvl = L1, srcs = maps:put(S, {W, A1}, Ss) }).

%% only trigger output if we've reached our threshold
maybe_fire(L, #neuron{ lvl = L } = Neuron) -> Neuron;
maybe_fire(_, #neuron{ lvl = L, thld = T, af = F, tgts = Ts } = Neuron)
  when T == none; L >= T ->
    Msg = {self(), activity, F(L)},
    [N ! Msg || N <- Ts],
    Neuron;
maybe_fire(L0, #neuron{ thld = T, tgts = Ts } = Neuron)
  when L0 == none; L0 >= T ->
    Msg = {self(), activity, 0.0},
    [N ! Msg || N <- Ts],
    Neuron;
maybe_fire(_, Neuron) -> Neuron.

backup(#neuron{ srcs = Srcs }) ->
    #{ sources => maps:to_list(
                    maps:map(
                      fun (_P, {W, _A}) -> W end,
                      Srcs))
     }.


-ifdef(TEST).
%%%----------------------------------------
%%% Test
%%%----------------------------------------

dbg() ->
    dbg:tracer(),
    dbg:p(all, call),
    dbg:tpl(?MODULE, []).

test_neuron() ->
    test_neuron(#{}).

test_neuron(Opts) ->
    N = new(maps:merge(#{ af => hardlim }, Opts)),
    enn_node:add_source(N, self(), 1.0),
    enn_node:add_target(N, self()),
    N.

test_activity(N, A) ->
    receive
        {N, activity, A} -> ok;
        {N, activity, O} ->
            {unexpected_activity, {expected, A}, {actual, O}}
    after 10 ->
            no_activity
    end.

stable_activity(N, A) ->
    stable_activity(N, A, no_activity).

stable_activity(N, A, L) ->
    case test_activity(N, A) of
        no_activity -> L;
        O -> stable_activity(N, A, O)
    end.
    
identity_test() ->
    N = test_neuron(),
    ok = enn_node:activate(N, 1.0),
    ok = test_activity(N, 1.0).

nochange_test() ->
    N = test_neuron(),
    ok = enn_node:activate(N, 0.5),
    ok = test_activity(N, 1.0),
    ok = enn_node:activate(N, 0.5),
    no_activity = test_activity(N, none).

threshold_test() ->
    N = test_neuron(#{ thld => 0.5 }),
    ok = enn_node:activate(N, 0.49),
    ?assertMatch(ok, test_activity(N, 0.0)),
    ok = enn_node:activate(N, 0.5),
    ok = test_activity(N, 1.0),

    %% check that we get notified also when the activity stops
    ok = enn_node:activate(N, 0.4),
    ?assertMatch(ok, test_activity(N, 0.0)).
    
    
xor_test() ->
    %% create xor network neurons
    [X1, X2]=Xs = [new(#{ af => hardlim }) || _ <- [x1, x2]],
    [Z, Y] = [new(#{ af => hardlim, thld => 0.5 }) || _ <- [z, y]],
    %% connect neurons
    [ok = enn_node:add_source(X, self(), 1.0) || X <- Xs],
    [ok = enn_node:add_source(Z, X, 0.4) || X <- Xs],
    [ok = enn_node:add_source(Y, X, 0.7) || X <- Xs],
    ok = enn_node:add_source(Y, Z, -1.0),
    ok = enn_node:add_target(Y, self()),
    %% run tests
    [begin
         ok = enn_node:activate(X1, I1),
         ok = enn_node:activate(X2, I2),
         case stable_activity(Y, O) of
             ok -> ok;
             E ->
                 throw({E, {I1, I2, O}})
         end
     end || {[I1, I2], O} <- 
                [{[0.0,1.0], 1.0},
                 {[0.0,0.0], 0.0},
                 {[1.0,0.0], 1.0},
                 {[1.0,1.0], 0.0}]
    ].


-endif.
