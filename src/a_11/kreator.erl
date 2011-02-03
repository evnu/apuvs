-module(kreator).
-export([start/1]).

%%%%%%
% Main entry point to the testsuite
%  You can call start from the command line (e.g. sh) using make run NUMBER_OF_PROCESSES=10 or by
%  calling it directly from the erl command line.
%
start([String]) when is_list (String) ->
    start(list_to_integer(String));

start(N) when is_integer(N) ->
    create(N, round(N/2), round(N/4)).

create (N, F, L) when is_integer(N) and is_integer(F) and is_integer(L) ->
    % Initialize a collector to build the msc
    C = collector:start_collector(),
    % Calculate majority
    Majority = floor(N / 2) + 1,
    % create N proposers
    Proposers = [spawn (proposer, initialize, [C]) || _ <- lists:seq(1,F)],
    % create F acceptors
    Acceptors = [spawn (acceptor, initialize, [C]) || _ <- lists:seq(1,N)],
    % create L learners
    Learners  = [spawn (learner , initialize, [self (), C, Majority]) || _ <- lists:seq(1,L)],

    [Acc ! {Learners, self()} || Acc <- Acceptors],
    [Prop ! {Acceptors, self()} || Prop <- Proposers],

    % output the pids
    io:format("Proposers: ~w\nAcceptors: ~w\nLearners: ~w\n",[Proposers, Acceptors, Learners]), 
    testsuite (C, Proposers, Acceptors, Learners),
    ok
    . %%%%% END OF FUNCTION


%%%%%%
% Testsuite
%  run some tests on the implementation and log the output.
%
testsuite (Collector, Proposers, _Acceptors, Learners) ->
    simple_run (Collector, lists:nth(1,Proposers), Learners)
    . %%%%% END OF FUNCTION


%%%%%%
% Simple run
%  Simple run starts a consensus and then waits for all learners to learn about the
%  decision. InitialProposer is the only process which proposes.
simple_run (Collector, InitialProposer, Learners) ->
    InitialProposer ! {{propose, 10}, self()},
    %% wait for all learners
    [receive {learned_about_decision, Learner} -> true end || Learner <- Learners],
    Collector ! {c_print_to_file, "msc/simple_run.msc"},
    io:format("simple run finished\n")
    . %% END OF FUNCTION


%double_run_sametime(Collector, Proposers, Learners) ->
%    First = lists:nth(1, Proposers),
%    Second = lists:last(Proposers),


%%%%%%
% Double run
% Double run starts the Paxos algorithm on two nodes with different values
%simple_run (Collector, InitialProposers, Learners) ->
%    InitialProposer ! {{propose, 10}, self()},
%    %% wait for all learners
%    [receive {learned_about_decision, Learner} -> true end || Learner <- Learners],
%    Collector ! {c_print_to_file, "msc/simple_run.msc"},
%    io:format("simple run finished\n")
%    . %% END OF FUNCTION


%%%%%%
% Floor
%  We need the floor function.
%
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).


