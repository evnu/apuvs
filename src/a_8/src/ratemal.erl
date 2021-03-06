-module(ratemal).
-export([creator/1, initialization/2]).

initialization(Collector, Parent) ->
    MaekawaPid = spawn (maekawa, initialization, [Collector, self()]),
    Parent ! {MaekawaPid, self()},
    Collector ! {c_name_process, {self(), io_lib:format("Application Layer above ~s",[collector:convert_process_id(MaekawaPid)])}},
    raten (Collector, MaekawaPid).

raten(Collector, MaekawaPid) ->
    receive
        {access_critical_section, Sender} ->
            io:format("~w is asking for resources at ~w\n",[self(), MaekawaPid]),
            MaekawaPid ! {m_enter_cs, self()},
            receive
                {a_ok, MaekawaPid} -> 
                    io:format("~w got a_ok from ~w\n",[self(), MaekawaPid]),
                    Collector ! {c_collect, {MaekawaPid, self(),a_ok}}
            end,
            io:format ("~w is in critical section nowi\n", [self()]),
            MaekawaPid ! {m_exit_cs, self()},
            io:format ("~w left critical section nowi\n", [self()]),
            Sender ! {a_done, self()}
    end,
    raten(Collector, MaekawaPid).

creator(Anzahl) ->
    % create a collector to build msc trace
    C = collector:start_collector (),
    % create application layers
    [Initiator1, _Initiator2, Initiator3|_]= [spawn (ratemal, initialization, [C, self()]) || _ <- lists:seq(1, Anzahl)],
    % initialize maekawa processes and groups
    makeGroups([], Anzahl), 

    % test the functionality
    Initiator3 ! {access_critical_section, self()},
    Initiator1 ! {access_critical_section, self()},
    timer:sleep(1000),
    % wait for reply from Initiator1 and Initiator3
    receive 
        {a_done, Initiator1} ->
            io:format("~w is done\n", [Initiator1])
    end,
    receive 
        {a_done, Initiator3} ->
            io:format("~w is done\n", [Initiator3])
    end,

    % tell collector to print msc
    % C ! {c_print_to_file, "../doc/msc.msc"},
    C ! {c_stop}.

overlap ([], _, Acc) ->
    Acc;
overlap ([H|T], E, Acc) ->
    overlap (T, E, [[E|H]|Acc]).

overlap ([H|T]) ->
    [E |_] = H,
    overlap (T, E, [H]).

partition ([], Accum, _) -> 
    %% overlap 
    overlap (Accum);
partition (PidList, [H|T], Ideal) when length(PidList) < Ideal ->
    partition ([], [lists:append(H, PidList) | T], Ideal);
partition (PidList, Accum, Ideal) -> 
    {NewGroup, Tail} = lists:split(Ideal, PidList),
    partition (Tail, [NewGroup | Accum], Ideal).

makeGroups(PidList, 0) when length(PidList) > 0 ->
    Ideal = loor(math:sqrt(length(PidList))),
    GroupList = partition (PidList, [], Ideal),
    io:format("grouplist = ~w\n", [GroupList]),
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

