-module(enn_sensor).

-export([new/1]).

%%%----------------------------------------

%% activate using random numbers on each 'update' message received
new(rng) ->
    spawn_link(
      fun () ->
              rng_loop([])
      end).

%%%----------------------------------------

rng_loop(Targets) ->
    receive
        {T, target} ->
            rng_loop([T|Targets]);
        update ->
            [rng_update(T) || T <- Targets],
            rng_loop(Targets);
        {R, backup} ->
            R ! {self(), backup, #{}}
    end.

rng_update(Target) ->
    io:format("rng update ~p~n", [Target]),
    Target ! {self(), activity, random:uniform()}.
