-module(ratemal).
-export([creator/1, initialization/1]).

initialization(Parent) ->
    MaekawaPid = spawn (maekawa, initialization, [self()]),
    Parent ! {MaekawaPid, self()},
    raten (MaekawaPid).

raten(MaekawaPid) ->
    % TODO erlang random numbers aren't worth shit.
    % random isn't random.
    timer:sleep(random:uniform(1000)),
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    case random:uniform(2) of
        2 -> 
            timer:sleep(100);
        1 ->
            MaekawaPid ! {m_enter_cs, self()},
            receive
                {a_ok, _} -> ok
            end,
            io:format ("~B is in critical section nowi\n", self()),
            MaekawaPid ! {m_exit_cs, self()},
            io:format ("~B left critical section nowi\n", self())
    end,
    raten(MaekawaPid).

creator(Anzahl) ->
    [spawn (ratemal, initialization, [self()]) || _ <- lists:seq(1, Anzahl)],
    makeGroups([], Anzahl), 
    started.

% TODO do we need this case?
partition ([], Accum, _) -> Accum;
% TODO comment me
% awesome hackery! 
% XXX the following is obvious.
partition ([_|PidList], [H|T], Ideal) when length(PidList) < Ideal ->
    [lists:append(H, PidList) | T];
partition (PidList, Accum, Ideal) -> 
    {NewGroup, Tail} = lists:split(Ideal, PidList),
    % guarantee overlap
    [HeadOfTail | _ ] = Tail,
    CombinedGroup = [HeadOfTail | NewGroup],
    partition (Tail, [CombinedGroup | Accum], Ideal).

makeGroups(PidList, 0) when length(PidList) > 0 ->
    Ideal = loor(math:sqrt(length(PidList))),
    GroupList = partition (PidList, [], Ideal),
    [[Pid ! {m_group, Group}|| {Pid,_} <- Group]|| Group <- GroupList]
    ;

makeGroups(PidList, Anzahl) ->
    receive
        MaekawaPid ->
            makeGroups([MaekawaPid | PidList], Anzahl - 1)
    end.

% WHY OH WHY...
loor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
loor(X) -> 
    trunc(X).

