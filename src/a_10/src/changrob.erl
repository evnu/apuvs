-module(changrob).
-export([initcr/1, cr/3]).

%%%%%%
% Initialize a chang-roberts process
% To initialize, usw spawn (changrob, initcr,[C]), where C is an et_collector. Then, send the ID of the 
% successor in the circle.
%
initcr(Collector) ->
    receive
        {cr_next_pid, Next} ->
            cr(Next, false, Collector)
    end.

%%%%%%
% cr main function
% Next - Id of the next chang-roberts process in the circle
%   NOTE: we ignore the case that Next could be crashed. 
% Participant - Boolean flag wether this process took part in a vote
% Collector - et_collector eventtracer
%
cr(Next, Participant, Collector) ->
    receive
        % we are told to start an election
        {start_election, _} ->
            io:fwrite("~w: starting election\n", [self()]),
            Collector ! {c_state_change, {self(), start_election}},

            Next ! {{election, self()}, self()},
            cr(Next, true, Collector);

        % we receive an election message
        {{election, Pred}, Sender}->
            Collector ! {c_collect, {Sender, self(), io_lib:format("<election, ~w>", [Pred])}},

            % le (X,Y) = {true if X < Y, equal if X == Y, otherwise false
            case (le(self(), Pred)) of
                true ->
                    % our id is < than Pred. Therefore, we propagate the value Pred with a
                    % new election message
                    io:fwrite("~w: received <election, ~w>...passing it on\n",[self(), Pred]),

                    Next ! {{election, Pred}, self()},
                    cr(Next, true, Collector);
                false ->
                    % our id is > than pred
                    if
                        % if we already participated, we discard the message
                        Participant ->
                            discardMsg;
                        % if we didn't participate, we vote for ourselve
                        not Participant ->
                            io:fwrite("~w: received <election, ~w>...proposing myself\n",[self(), Pred]),
                            Next ! {{election, self()}, self()}
                    end,
                    cr(Next, Participant, Collector);
                equal ->
                    % we reached a decision. let's tell everybody about it
                    io:fwrite("~w: received <election, ~w>...i was elected\n",[self(), Pred]),

                    Next ! {{elected, self()}, self()},
                    cr(Next, false, Collector)
            end;

        % we receive an elected message
        {{elected, Leader}, Pred} ->
            %report the leader to somewhere
            case (self() == Leader) of
                false ->
                    % propagate the elected message
                    io:fwrite("~w: elected ~w to be the leader\n", [self(),
                            Leader]),
                    Collector ! {c_collect, {Pred, self(), io_lib:format("<elected, ~w>", [Leader])}},
                    Next ! {{elected, Leader}, self()},
                    cr(Next, false, Collector);
                true ->
                    % the elected message went around the circle. let's abort.
                    io:fwrite("~w: elected ~w to be the leader\n", [self(),
                            Leader]),
                    Collector ! {c_collect, {Pred, self(), io_lib:format("<elected, ~w>", [Leader])}},
                    Collector ! {c_state_change, {self(), leader}},
                    cr(Next, false, Collector)
            end
    end.

%%%%%%
% Helper function
% compare two values. return true if X < Y, false if X > Y, equal otherwise.
le (X,X) -> equal;
le (X,Y) -> X < Y.
