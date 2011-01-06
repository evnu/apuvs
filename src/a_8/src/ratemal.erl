-module(ratemal).
-export([creator/1, initialization/1]).

initialization(Parent) ->
    MaekawaPid = spawn (maekawa, initialization, [self()]),
    Parent ! {MaekawaPid, self()},
    raten (MaekawaPid).

raten(MaekawaPid) ->
    Geraten = random:uniform(2),
    case Geraten rem 2 of
        2 -> 
            timers:sleep(100);
        1 ->
            MaekawaPid ! {m_enter_cs, self()},
            io:format ("~B is in critical section nowi\n", self()),
            MaekawaPid ! {m_exit_cs, self()}
    end,
    raten(MaekawaPid).

creator(Anzahl) ->
    [spawn (ratemal, initialization, [self()]) || _ <- lists:seq(1, Anzahl)],
    makeGroups([], Anzahl), 
    started.

% TODO do we need this case?
partition ([], Accum, _) -> Accum;
% TODO comment me
partition (PidList, Accum, Ideal) when length(PidList) < Ideal ->
    % awesome hackery! 
    % XXX the following is obvious.
    [[0|PidList] | Accum];
partition (PidList, Accum, Ideal) -> 
    {NewGroup, Tail} = lists:split(Ideal, PidList),
    % guarantee overlap
    [HeadOfTail | _ ] = Tail,
    CombinedGroup = [HeadOfTail | NewGroup],
    partition (Tail, [CombinedGroup | Accum], Ideal).

makeGroups(PidList, 0) when length(PidList) > 0 ->
    Ideal = loor(math:sqrt(length(PidList))),
    GroupList = partition (PidList, [], Ideal),
    [[Pid ! {m_group, Group} || {Pid,_} <- Group]|| [_Head|Group] <- GroupList]
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

