-module(enn_node).

%% API
-export([new/2]).

%% Internal
-export([run/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
          activate :: fun((number()) -> number()),
          links = [] :: list({pid(), number()}),
          time = 0 :: integer(),
          level = 0 :: number()
         }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Fun, FwdLinks) ->
    State = create(Fun, FwdLinks),
    spawn_link(?MODULE, run, [State]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(State) ->
    receive
        {activity, Activity} ->
            State1 = trigger(Activity, State),
            run(State1);
        _Other ->
            run(State)
    end.

create(Fun, FwdLinks)
  when is_function(Fun, 1), is_list(FwdLinks) ->
    #state{
       activate = Fun,
       links = [{Node, W} || {Node, W} <- FwdLinks, is_pid(Node), W /= 0]
      }.

trigger(Activity, State0) ->
    State = register_activity(Activity, State0),
    Output = (State#state.activate)(State#state.level),
    if Output /= 0 ->
            Time = State#state.time + 1,
            [Node ! {activity, {Time, Output * W}} || {Node, W} <- State#state.links],
            State;
       true ->
            State
    end.
        
register_activity({Time, Level}, State0) ->
    State = #state{ level = Level0 } = set_time(Time, State0),
    State#state{ level = Level0 + Level }.

set_time(Time, #state{ time = Time0 } = State) when Time > Time0 ->
    State#state{ time = Time, level = 0 };
set_time(_, State) -> State.


-ifdef(TEST).

test_node(Links) ->
    F = fun (I) ->
                if I >= 1 -> 1;
                   true -> 0
                end
        end,
    N = create(F, Links).

test_activity(Activity) ->
    receive
        {activity, Activity} -> ok;
        Other -> throw({activity, {expected, Activity}, {actual, Other}})
    after
        10 -> none
    end.
    
create_node_test() ->
    N = test_node([]),
    ?assertMatch({state, _, [], 0, 0}, N).

time_test() ->
    [begin
         N = set_time(T, #state{ time = 5, level = 123 }),
         ?assertMatch(#state{ time = T1, level = L }, N)
     end || {T, T1, L} <- [{1, 5, 123}, {5, 5, 123}, {8, 8, 0}]].

trigger_node_test() ->
    N = test_node([{self(), 1.3}]),
    trigger({5, 1.2}, N),
    ?assertMatch(ok, test_activity({6, 1.3})).

multi_activation_test() ->
    N = lists:foldl(
          fun (A, N0) ->
                  N = trigger({4, A}, N0),
                  ?assertMatch(none, test_activity(none)),
                  N
          end,
          test_node([{self(), 1.5}]),
          [0.2, 0.3, 0.4]),
    trigger({4, 0.1}, N),
    ?assertMatch(ok, test_activity({5, 1.5})).


-endif.
