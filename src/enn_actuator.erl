-module(enn_actuator).

-export([stdout/0]).

stdout() ->
    spawn_link(
      fun () ->
              stdout_loop([])
      end).

stdout_loop(Sources) ->
    receive
        {S, source} ->
            stdout_msg("accept input from ~p~n", [S]),
            stdout_loop([S|Sources]);
        {S, activity, A} ->
            case [S] -- Sources of
                [] -> stdout_msg("activity from ~p: ~p~n", [S, A]);
                _  -> stdout_msg("unexpected activity from: ~p (~p)~n", [S, A])
            end,
            stdout_loop(Sources)
    end.

stdout_msg(Fmt, Args) ->
    io:format("~s/stdout[~p]: " ++ Fmt, [?MODULE, self()|Args]).
