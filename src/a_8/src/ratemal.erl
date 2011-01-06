-module(ratemal).
-export([creator/1]).

initialization(Parent) ->
    MaekawaPid = spawn (maekawa, initialization, [self()]),
    Parent ! {MaekawaPid, self()},
    raten (MaekawaPid).

raten(MaekawaPid) ->
    Geraten = random:uniform(2),
    case Geraten rem 2 of
        2 -> 
            sleep(100);
        1 ->
            MaekawaPid ! {m_enter_cs, self()},
            io:format ("~B is in critical section nowi\n", self()),
            MaekawaPid ! {m_exit_cs, self()}
    end
    raten(MaekawaPid).

creator(Anzahl) ->
    [spawn (ratemal, initialization, [self()]) || _ <- lists:seq(1, Anzahl)],
    makeGroups([], Anzahl).

makeGroups(PidList, 0) ->
    Ideal = sqrt(length(PidList)),

makeGroups(PidList, Anzahl) ->
    receive
        MaekawaPid ->
            makeGroups([MaekawaPid | PidList], Anzahl - 1)
    end.
