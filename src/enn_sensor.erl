-module(enn_sensor).

-export([rng/0]).

rng() ->
    spawn_link(
      fun () ->
              rng_loop([])
      end).

rng_loop(Targets) ->
    receive
        {T, target} ->
            rng_loop([T|Targets]);
        update ->
            [rng_update(T) || T <- Targets],
            rng_loop(Targets)
    end.

rng_update(Target) ->
    io:format("rng update ~p~n", [Target]),
    Target ! {self(), activity, random:uniform()}.
