-module(test).
-export([testcr/1]).

testcr(N)->
    % create a collector to build msc trace
    C = collector:start_collector(),
    [H|T] = Pids = [spawn(changrob, initcr, [C]) || _ <- lists:seq(1, N)],
    SendTupels = lists:zip(Pids, lists:append(T, [H])),
    lists:map(fun ({Pred, Next}) -> Pred ! Next end, SendTupels),
    H ! {start_election, self()},
    timer:sleep (1000),
    C ! {c_print_to_file, "single.msc"},
    C ! {c_clear_cache},

    [_,_,H2|_] = T,
    H ! {start_election, self()},
    H2 ! {start_election, self()},
    timer:sleep (1000),
    C ! {c_print_to_file, "double.msc"},
    ok.
