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
    testsuite(N)
    .

create (N, F, L) when is_integer(N) and is_integer(F) and is_integer(L) ->
    % Initialize a collector to build the msc
    C = collector:start_collector(),
    % Calculate majority
    Majority = floor(N / 2) + 1,
    % create N proposers
    Proposers = [spawn (proposer, initialize, [C, Majority]) || _ <- lists:seq(1,F)],
    % create F acceptors
    Acceptors = [spawn (acceptor, initialize, [C]) || _ <- lists:seq(1,N)],
    % create L learners
    Learners  = [spawn (learner , initialize, [self (), C, Majority]) || _ <- lists:seq(1,L)],

    % output the pids
    io:format("Proposers: ~w\nAcceptors: ~w\nLearners: ~w\n",[Proposers, Acceptors, Learners]), 
    {Proposers, Acceptors, Learners, C}
    . %%%%% END OF FUNCTION


%%%%%%
% Testsuite
%  run some tests on the implementation and log the output.
%
testsuite (N) ->
    simple_run (N),
    timer:sleep(1000),
    io:fwrite("\n",[]),
    double_run_sametime (N),
    timer:sleep(1000),
    io:fwrite("\n",[]),
    double_run_minority (N),
    timer:sleep(1000),
    io:fwrite("\n",[]),
    double_run_late (N)
    . %%%%% END OF FUNCTION


%%%%%%
% Simple run
%  Simple run starts a consensus and then waits for all learners to learn about the
%  decision. InitialProposer is the only process which proposes.
simple_run (N) ->
    {Proposers, A, L, C} = create(N, round(N/2), round(N/4)),
    [Acc ! {L, self()} || Acc <- A],
    [Prop ! {A, self()} || Prop <- Proposers],

    lists:nth(1, Proposers) ! {{propose, 10}, self()},
    %% wait for all learners
    [receive {learned_about_decision, Learner} -> true end || Learner <- L],
    C ! {c_print_to_file, "msc/simple_run.msc"},
    io:format("simple run finished\n")
    . %% END OF FUNCTION

double_run_sametime(N) ->
    {Proposers, A, L, C} = create(N, round(N/2), round(N/4)),
    [Acc ! {L, self()} || Acc <- A],
    {Proposers_first, Proposers_second} = lists:split(2, Proposers),
    {Acceptors_first, Acceptors_second} = lists:split(6, A),
    [Prop ! {Acceptors_first, self()} || Prop <- Proposers_first],
    [Prop ! {A, self()} || Prop <- Proposers_second],

    First = lists:nth(1, Proposers),
    Second = lists:last(Proposers),
    First ! {{propose, 4}, self()},
    Second ! {{propose, 7}, self()},
    %% wait for all learners
    [receive {learned_about_decision, Learner} -> true end || Learner <- L],
    C ! {c_print_to_file, "msc/double_run.msc"},
    io:format("double run finished\n")
    . %% END OF FUNCTION

double_run_minority(N) ->
    {Proposers, A, L, C} = create(N, round(N/2), round(N/4)),
    [Acc ! {L, self()} || Acc <- A],
    {Proposers_first, Proposers_second} = lists:split(2, Proposers),
    {Acceptors_first, Acceptors_second} = lists:split(6, A),
    [Prop ! {Acceptors_second, self()} || Prop <- Proposers_first],
    [Prop ! {Acceptors_second, self()} || Prop <- Proposers_second],

    First = lists:nth(1, Proposers),
    Second = lists:last(Proposers),
    First ! {{propose, 4}, self()},
    Second ! {{propose, 7}, self()},
    %% wait for all learners
    [receive {learned_about_decision, Learner} -> true after 1000 -> true end || Learner <- L],
    C ! {c_print_to_file, "msc/double_run_min.msc"},
    io:format("double run minority finished\n")
    . %% END OF FUNCTION

double_run_late(N) ->
    {Proposers, A, L, C} = create(N, round(N/2), round(N/4)),
    [Acc ! {L, self()} || Acc <- A],
    {Proposers_first, Proposers_second} = lists:split(2, Proposers),
    {Acceptors_first, Acceptors_second} = lists:split(6, A),
    [Prop ! {A, self()} || Prop <- Proposers_second],
    timer:sleep(1000),
    [Prop ! {Acceptors_first, self()} || Prop <- Proposers_first],

    First = lists:nth(1, Proposers),
    Second = lists:last(Proposers),
    First ! {{propose, 4}, self()},
    Second ! {{propose, 7}, self()},
    %% wait for all learners
    [receive {learned_about_decision, Learner} -> true end || Learner <- L],
    C ! {c_print_to_file, "msc/double_run_late.msc"},
    io:format("double run late finished\n")
    . %% END OF FUNCTION


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


