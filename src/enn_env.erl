-module(enn_env).

-export([new/0, connect/2, connect/3, update/3]).

%%%----------------------------------------

new() ->
    spawn_link(
      fun () ->
              env_loop([])
      end).

connect(NN, Env) when is_map(NN), is_pid(Env) ->
    maps:fold(
      fun (Node, #{ type := {sensor, [{env, Key}]} }, ok) ->
              connect(Node, Key, Env);
          (Node, #{ type := {actuator, [{env, _Key}]} }, ok) ->
              enn_node:add_target(Node, Env);
          (_, _, Acc) -> Acc
      end, ok, NN).

connect(Pid, Key, Env) when is_pid(Pid), is_atom(Key), is_pid(Env) ->
    Env ! {Pid, target, Key},
    ok.

update(Key, Value, Env) when is_atom(Key), is_pid(Env) ->
    Env ! {self(), env, Key, enn_node:to_weight(Value)},
    ok.


%%%----------------------------------------

env_loop(Ts) ->
    receive
        {T, target, Key} ->
            env_loop([{T, Key}|Ts]);
        {_S, env, Key, Value} ->
            [T ! {self(), env, Key, Value} || {T, K} <- Ts, K == Key],
            env_loop(Ts);
        _Msg ->
            env_loop(Ts)
    end.
