-module(kreator).
-export(start/1).

start(N) ->
    create(round(N/2), N, round(N/4)).

create (N, F, L) when is_integer(N) and is_integer(F) and is_integer(L) ->
    % Initialize a collector to build the msc
    C = collector:start_collector(),
    % create N proposers
    Proposers = [spawn (proposer, initialize, [C]) || _ <- lists:seq(1,N)],
    % create F acceptors
    Acceptors = [spawn (acceptor, initialize, [C]) || _ <- lists:seq(1,F)],
    % create L learners
    Learners  = [spawn (learner , initialize, [C]) || _ <- lists:seq(1,L)],

    [Acc ! {Learners, self()} || Acc <- Acceptors],
    [Prop ! {Acceptors, self()} || Prop <- Proposers]

    . %%%%% END OF FUNCTION
