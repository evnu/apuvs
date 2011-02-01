-module(learner).
-export([initialize/2]).

%%%%%%%
%
% Initialize a learner
%

initialize (Collector, Maj) ->
    learner(Collector, Maj, 1, 0)
    . %% END OF FUNCTION

learner(Collector, Maj, OldR , OldNum) ->
    {FinalNewRound,FinalNewNum} = 
    receive
        {{accepted, Round, _Value}, _Sender} ->
            {TempRound, TmpAccepted} =
            if (Round > OldR) -> 
                    {Round, 0};
                true ->
                    {OldR, OldNum}
            end,
            NumAccepted = TmpAccepted + 1,
            if (NumAccepted == Maj) ->
                    %%% TODO we decided. let's change the state
                    true;
                true -> false
            end,
            {TempRound, NumAccepted} % return the local variables
    end,
    learner (Collector, Maj, FinalNewRound, FinalNewNum)
    . %% END OF FUNCTION
