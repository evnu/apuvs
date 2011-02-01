-module(learner).
-export([initialize/3]).

%%%%%%%
%
% Initialize a learner
%

initialize (Creator, Collector, Maj) ->
    Collector ! {c_name_process, {self(), "Learner"}},
    learner(Creator, Collector, Maj, 1, 0)
    . %% END OF FUNCTION

learner(Creator, Collector, Maj, OldR , OldNum) ->
    {FinalNewRound,FinalNewNum} = 
    receive
        {{accepted, Round, Value}, Sender} ->
            io:format("learner ~w <accepted, Round = ~w, Value = ~w> from ~w\n", [self(), Round,
                    Value, Sender]),
            Collector ! {c_collect, {Sender, self(), io_lib:format("<accepted, ~w,~w>", [Round, Value])}},
            % track the message
            Collector ! {c_collect, {Sender, self(), io_lib:format("<accepted, ~w, ~w>", [Round, Value])}},
            {TempRound, TmpAccepted} =
            if (Round > OldR) -> 
                    {Round, 0};
                true ->
                    {OldR, OldNum}
            end,
            NumAccepted = TmpAccepted + 1,
            io:format("NumAccepted = ~w, Maj = ~w\n", [NumAccepted, Maj]),
            if (NumAccepted == Maj) ->
                    Collector ! {c_state_change, {self(), io_lib:format("decided on ~w", [Value])}},
                    io:format("~w: we all decided on ~w\n",[self(), Value]),
                    % tell creator that we are finished to enable building the msc
                    Creator ! {learned_about_decision, self()};
                true -> false
            end,
            {TempRound, NumAccepted} % return the local variables
    end,
    learner (Creator, Collector, Maj, FinalNewRound, FinalNewNum)
    . %% END OF FUNCTION
