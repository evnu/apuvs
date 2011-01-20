-module(changrob).
-export([initcr/1, cr/3]).

initcr(Collector) ->
    receive
        Next ->
            cr(Next, false, Collector)
    end.

cr(Next, Participant, Collector) ->
    receive
        {start_election, _} ->
            io:fwrite("~w: starting election\n", [self()]),
            Collector ! {c_state_change, {self(), start_election}},

            Next ! {{election, self()}, self()},
            cr(Next, true, Collector);
        {{election, Pred}, Sender}->
            Collector ! {c_collect, {Sender, self(), io_lib:format("<election, ~w>", [Pred])}},
            case (le(self(), Pred)) of
                true ->
                    io:fwrite("~w: received <election, ~w>...passing it on\n",[self(), Pred]),

                    Next ! {{election, Pred}, self()},
                    cr(Next, true, Collector);
                false ->
                    if
                        Participant ->
                            discardMsg;
                        not Participant ->
                            io:fwrite("~w: received <election, ~w>...proposing myself\n",[self(), Pred]),
                            Next ! {{election, self()}, self()}
                    end,
                    cr(Next, Participant, Collector);
                equal ->
                    io:fwrite("~w: received <election, ~w>...i was elected\n",[self(), Pred]),

                    Next ! {{elected, self()}, self()},
                    cr(Next, false, Collector)
            end;
        {{elected, Leader}, Pred} ->
            %report the leader to somewhere
            case (self() == Leader) of
                false ->
                    io:fwrite("~w: elected ~w to be the leader\n", [self(),
                            Leader]),
                    Collector ! {c_collect, {Pred, self(), io_lib:format("<elected, ~w>", [Leader])}},
                    Next ! {{elected, Leader}, self()},
                    cr(Next, false, Collector);
                true ->
                    io:fwrite("~w: elected ~w to be the leader\n", [self(),
                            Leader]),
                    Collector ! {c_collect, {Pred, self(), io_lib:format("<elected, ~w>", [Leader])}},
                    Collector ! {c_state_change, {self(), leader}},
                    cr(Next, false, Collector)
            end
    end.

% compare to values. return true if X < Y, false if X > Y, equal otherwise.
le (X,X) -> equal;
le (X,Y) -> X < Y.
