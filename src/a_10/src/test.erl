-module(test).
-export([testcr/1]).

%%%%%%
% testcr (N)
%  test the changrob module with N processes
%

testcr([String]) when is_list(String) ->
    testcr(list_to_integer(String));

testcr(N) when is_integer (N) ->
    % create a collector to build msc trace
    C = collector:start_collector(),
    % initialize the chang-roberts processes
    [H|T] = Pids = [spawn(changrob, initcr, [C]) || _ <- lists:seq(1, N)],
    % tell them about their successor
    SendTupels = lists:zip(Pids, lists:append(T, [H])),
    lists:map(fun ({Pred, Next}) -> Pred ! {cr_next_pid, Next} end, SendTupels),

    % start an election at each process
    [start_single_election (lists:nth(I, Pids), C) || I <- lists:seq(1,N)],

    % start two concurrent elections
    start_concurrent_elections([s(1,Pids), s(2,Pids)], C),
    start_concurrent_elections([s(1,Pids), s(3, Pids), s(5, Pids)], C),
    start_concurrent_elections([s(2,Pids), s(3, Pids), s(5, Pids)], C),
    start_concurrent_elections([s(3,Pids), s(4, Pids), s(5, Pids)], C),
    ok.

%%%%% shorter wrapper for lists:nth (..)
s(N,L) -> lists:nth(N,L).

start_single_election(Pid, C) ->
    Pid ! {start_election, self()},
    timer:sleep (1000),
    C ! {c_print_to_file, io_lib:format("msc/single_election_at_~s.msc",[collector:convert_process_id(Pid)])},
    C ! {c_clear_cache}.

start_concurrent_elections(Pids, C) ->
    [ P ! {start_election, self()} || P <- Pids],
    timer:sleep(1000),
    C ! {c_print_to_file, 
        io_lib:format("msc/concurrent_with_~s.msc",
            [lists:map(fun(P) -> collector:convert_process_id(P) end, Pids)])
        },
    C ! {c_clear_cache}.


