-module(learner).
-export([initialize/1]).

%%%%%%%
%
% Initialize a learner
%

initialize (Collector, Maj) ->
    learner(Collector, Maj, 1, 0)
    . %% END OF FUNCTION

learner(Collector, Maj, R , Num) ->
    if
        Num >= Maj ->
            decided(
    receive
        {R, V, _} ->
            learner(Collector, Maj, R, Num + 1);
        {Rother, _, _} ->
            learner(Collector, Maj, Rother, 0)
    end.
