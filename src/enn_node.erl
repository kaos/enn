-module(enn_node).

%% Neuron node
%%
%% Start with initial actuator, threshold and no inputs/outputs.
%%
%% Register inputs with {Pid, Weight} tuples. Each input will receive
%% a register output message.
%%
%% Send activation as soon as it change.
%%

%% API
-export([new/2, new/3, add_source/3, add_target/2, activate/2]).

%% Internal
-export([run/1]).

-ifdef(TEST).
-export([dbg/0]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(node, {
          id :: term(),
          af :: fun((float()) -> float()), %% actuator function
          lvl = 0.0 :: float(), %% activation level
          thld = 0.0 :: float(),
          srcs = #{}, %% input sources
          tgts = [] :: [pid()] %% output targets
         }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Id, AF) -> new(Id, AF, 0.0).

new(Id, AF, Thld) when is_number(Thld) ->
    Node = #node{
              id = Id,
              af = create_activator(AF),
              thld = to_weight(Thld)
             },
    spawn_link(?MODULE, run, [Node]).


add_source(N, S, W) when is_pid(N), is_pid(S) ->
    N ! {S, source, to_weight(W)},
    add_target(S, N).

add_target(N, T) when is_pid(N), is_pid(T) ->
    N ! {T, target},
    ok.

activate(N, A) when is_pid(N), is_float(A) ->
    N ! {self(), activity, A},
    ok;
activate(N, A) when is_pid(N) ->
    activate(N, to_weight(A)).


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_activator(Fun) when is_function(Fun, 1) -> Fun;
create_activator(Std) when is_atom(Std) -> fun enn_f:Std/1;
create_activator({M, F}) -> fun M:F/1;
create_activator({M, F, A}) -> create_activator(apply(M, F, A));
create_activator(L) when is_list(L) ->
    lists:foldl(
      fun (A, F) -> F(create_activator(A)) end,
      fun (F) -> F end, L).
    
to_weight(N) when is_number(N) -> float(N);
to_weight(random) -> random:uniform() - random:uniform();
to_weight({M, F, A}) -> to_weight(apply(M, F, A)).

run(Node) ->
    receive
        {S, activity, A} when is_pid(S), is_float(A) ->
            run(process(A, S, Node));
        {S, source, W} when is_pid(S), is_float(W) ->
            run(Node#node{ srcs = maps:put(S, {W, 0.0}, Node#node.srcs) });
        {T, target} when is_pid(T) ->
            run(Node#node{ tgts = [T|Node#node.tgts] })
    end.

process(A, S, #node{ lvl = L0, srcs = Ss } = Node) ->
    {W, A0} = maps:get(S, Ss),
    case A * W of
        A0 -> Node;
        A1 ->
            L1 = L0 + A1 - A0,
            maybe_fire(L0, Node#node{ lvl = L1, srcs = maps:put(S, {W, A1}, Ss) })
    end.

%% only trigger output if we've reached our threshold
maybe_fire(_, #node{ lvl = L, thld = T, tgts = Ts, af = F } = Node)
  when L >= T ->
    Msg = {self(), activity, F(L)},
    [N ! Msg || N <- Ts],
    Node;
maybe_fire(L0, #node{ thld = T, tgts = Ts } = Node)
  when L0 >= T ->
    Msg = {self(), activity, 0.0},
    [N ! Msg || N <- Ts],
    Node;
maybe_fire(_, Node) -> Node.


-ifdef(TEST).

dbg() ->
    dbg:tracer(),
    dbg:p(all, call),
    dbg:tpl(?MODULE, []).

test_node() ->
    test_node(#{}).

test_node(Opts) ->
    O = maps:merge(#{ threshold => 0.0 }, Opts),
    N = new(test, hardlim, maps:get(threshold, O)),
    add_source(N, self(), 1.0),
    add_target(N, self()),
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
    N = test_node(),
    ok = activate(N, 1.0),
    ok = test_activity(N, 1.0).

nochange_test() ->
    N = test_node(),
    ok = activate(N, 0.5),
    ok = test_activity(N, 1.0),
    ok = activate(N, 0.5),
    no_activity = test_activity(N, none).

threshold_test() ->
    N = test_node(#{threshold => 0.5}),
    ok = activate(N, 0.49),
    ?assertMatch(no_activity, test_activity(N, none)),
    ok = activate(N, 0.5),
    ok = test_activity(N, 1.0),

    %% check that we get notified also when the activity stops
    ok = activate(N, 0.4),
    ?assertMatch(ok, test_activity(N, 0.0)).
    
    
xor_test() ->
    %% create xor network nodes
    [X1, X2]=Xs = [new(N, hardlim) || N <- [x1, x2]],
    [Z, Y] = [new(N, hardlim, 0.5) || N <- [z, y]],
    %% connect nodes
    [ok = add_source(X, self(), 1.0) || X <- Xs],
    [ok = add_source(Z, X, 0.4) || X <- Xs],
    [ok = add_source(Y, X, 0.7) || X <- Xs],
    ok = add_source(Y, Z, -1.0),
    ok = add_target(Y, self()),
    %% run tests
    [begin
         ok = activate(X1, I1),
         ok = activate(X2, I2),
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
