-module(changrob).
-export([initcr/0, cr/2]).

initcr() ->
    receive
        Next ->
            cr(Next, false)
    end.

cr(Next, Participant) ->
    receive
        {start_election, _} ->
            io:fwrite("~w: starting election\n", [self()]),
            Next ! {election, self()},
            cr(Next, true);
        {election, Pred} ->
            case (le(self(), Pred)) of
                true ->
                    io:fwrite("~w: received <election, ~w>...passing it on\n",[self(), Pred]),
                    Next ! {election, Pred},
                    cr(Next, true);
                false ->
                    if
                        Participant ->
                            discardMsg;
                        not Participant ->
                            io:fwrite("~w: received <election, ~w>...proposing myself\n",[self(), Pred]),
                            Next ! {election, self()}
                    end,
                    cr(Next, Participant);
                equal ->
                    io:fwrite("~w: received <election, ~w>...i was elected\n",[self(), Pred]),
                    Next ! {elected, self()},
                    cr(Next, false)
            end;
        {elected, Leader} ->
            %report the leader to somewhere
            case (self() == Leader) of
                false ->
                    io:fwrite("~w: elected ~w to be the leader\n", [self(),
                            Leader]),
                    Next ! {elected, Leader},
                    cr(Next, false);
                true ->
                    io:fwrite("~w: elected ~w to be the leader\n", [self(),
                            Leader]),
                    cr(Next, false)
            end
    end.

% compare to values. return true if X < Y, false if X > Y, equal otherwise.
le (X,X) -> equal;
le (X,Y) -> X < Y.
