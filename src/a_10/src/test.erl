-module(test).
-export([testcr/1]).

%%%%%%
% testcr (N)
%  test the changrob module with N processes
%
testcr(N)->
    % create a collector to build msc trace
    C = collector:start_collector(),
    % initialize the chang-roberts processes
    [H|T] = Pids = [spawn(changrob, initcr, [{cr_collector, C}]) || _ <- lists:seq(1, N)],
    % tell them about their successor
    SendTupels = lists:zip(Pids, lists:append(T, [H])),
    lists:map(fun ({Pred, Next}) -> Pred ! Next end, SendTupels),

    % start a first election
    H ! {start_election, self()},
    timer:sleep (1000),
    C ! {c_print_to_file, "single.msc"},
    C ! {c_clear_cache},

    % start two concurrent elections
    [_,_,H2|_] = T,
    H ! {start_election, self()},
    H2 ! {start_election, self()},
    timer:sleep (1000),
    C ! {c_print_to_file, "double.msc"},
    C ! {c_clear_cache},
    ok.
