-module(ratemal).
-export([creator/1, initialization/1]).

initialization(Parent) ->
    MaekawaPid = spawn (maekawa, initialization, [self()]),
    Parent ! {MaekawaPid, self()},
    raten (MaekawaPid).

raten(MaekawaPid) ->
    receive
        get_se_resource_biatch ->
            io:format("~w is asking for resources\n",[self()]),
            MaekawaPid ! {m_enter_cs, self()},
            receive
                {a_ok, MaekawaPid} -> ok
            end,
            io:format ("~w is in critical section nowi\n", [self()]),
            MaekawaPid ! {m_exit_cs, self()},
            io:format ("~w left critical section nowi\n", [self()])
    end,
    raten(MaekawaPid).

creator(Anzahl) ->
    [Initiator|_]= [spawn (ratemal, initialization, [self()]) || _ <- lists:seq(1, Anzahl)],
    makeGroups([], Anzahl), 
    Initiator ! get_se_resource_biatch,
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

