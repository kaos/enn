-module(enn_actuator).

-export([new/1]).

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
                _  -> stdout_msg("unexpected activity from: ~p (~p)~n", [S, A])
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
