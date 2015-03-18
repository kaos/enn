-module(enn_actuator).

-export([new/1]).

-record(env, { key, delay = 10, s = [], t = [] }).

%%%----------------------------------------

%% Dump activity to stdout
new(stdout) ->
    spawn_link(
      fun () ->
              stdout_loop([])
      end);

%% forward activity to pid
new({fwd, Pid}) ->
    spawn_link(
      fun () ->
              fwd_loop(Pid)
      end);

new({env, Key}) ->
    spawn_link(
      fun () ->
              env_loop(#env{ key = Key })
      end).


%%%----------------------------------------

stdout_loop(Sources) ->
    receive
        {S, source, _Weight} ->
            stdout_msg("accept input from ~p~n", [S]),
            stdout_loop([S|Sources]);
        {S, activity, A} ->
            case [S] -- Sources of
                [] -> stdout_msg("activity from ~p: ~p~n", [S, A]);
                _  -> stdout_msg("unexpected activity from ~p: ~p~n", [S, A])
            end,
            stdout_loop(Sources);
        {R, backup} ->
            R ! {self(), backup, #{}};
        Msg ->
            stdout_msg("unexpected message: ~p~n", [Msg]),
            stdout_loop(Sources)
    end.

stdout_msg(Fmt, Args) ->
    io:format("~p ~s/stdout[~p]: " ++ Fmt, [now(), ?MODULE, self()|Args]).

fwd_loop(Pid) ->
    receive
        Msg ->
            Pid ! Msg,
            fwd_loop(Pid)
    end.

env_loop(Env) ->
    env_loop(infinity, undefined, Env).

env_loop(Timeout, A0, Env) ->
    receive
        {S, activity, A} ->
            env_loop(Env#env.delay, [{S, A}], Env);
        {T, target} ->
            env_loop(Timeout, A0, Env#env{ t=[T|Env#env.t] });
        {S, source, W} ->
            env_loop(Timeout, A0, Env#env{ s=[{S, W}|Env#env.s] });
        _ ->
            env_loop(Timeout, A0, Env)
    after
        Timeout ->
            [[[enn_env:update(Env#env.key, A * W, T)
               || T <- Env#env.t]
              || {Sr, W} <- Env#env.s, Sr == S]
             || {S, A} <- A0],
            env_loop(Env)
    end.
