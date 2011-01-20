-module(test).
-export([testcr/1]).

testcr(N)->
    [H|T] = Pids = [spawn(changrob, initcr, []) || _ <- lists:seq(1, N)],
    SendTupels = lists:zip(Pids, lists:append(T, [H])),
    lists:map(fun ({Pred, Next}) -> Pred ! Next end, SendTupels),
    H ! {start_election, self()},
    ok.
