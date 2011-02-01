-module(paxos).
-export([create/3]).

create (N, F, L) when is_integer(N) and is_integer(F) and is_integer(L) ->
    % Initialize a collector to build the msc
    C = collector:start_collector(),
    % create N proposers
    Proposers = [spawn (proposer, initialize, [C]) || _ <- lists:seq(1,N)],
    % create F acceptors
    Acceptors = [spawn (acceptor, initialize, [C]) || _ <- lists:seq(1,F)],
    % create L learners
    Learners  = [spawn (learner , initialize, [C]) || _ <- lists:seq(1,L)]

    . %%%%% END OF FUNCTION
