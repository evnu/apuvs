-module(collector).
-export([test/0, collector/0, ping/1, start_collector/0, convert_process_id/1]).


start_collector () ->
    spawn (collector, collector, []).

collector () ->
    {ok, C} = et_collector:start_link ([]),
    collector (C).
    
collector (C) ->
    DONE = receive
        {c_collect, {Sender, Receiver, Message}} when is_atom(Message) ->
            et_collector:report_event(C, 1, Sender, Receiver, Message, []);
        {c_state_change, {Sender, State}} ->
            et_collector:report_event(C,1,Sender,Sender,state_change,[State]);
        {c_name_process, {Sender, Name}}  ->
            et_collector:report_event(C,1,Sender,Sender,name_process,[Name]);
        {c_print} ->
            io:format ("~s", [string_representation(C)]);
        {c_print_to_file, Filename} ->
            file:write_file(Filename, string_representation(C));
        {c_stop} ->
            true
    end,
    case DONE of 
        true -> ok;
        _ -> collector (C)
    end.

%%%%
% String represenation
% return a msc string representation
%
string_representation (C) ->
    Processes = iterate (C,
        fun({event,_,_,_,Sender,Receiver,_,_}, Acc) ->
                TempAcc = sets:add_element(Sender, Acc), % try to add the Sender
                sets:add_element(Receiver, TempAcc)  % try to add the Receiver
        end,
        sets:new ()),
    [LP|LT] = sets:to_list (Processes),

    "msc {\n hscale=2;\n"
    ++
    io_lib:format("\"~s\"", [convert_process_id (LP)]) 
    ++
    [io_lib:format(", \"~s\"",[convert_process_id(LPP)]) || LPP <- LT] 
    ++
    ";|||;\n"
    ++
    iterate (C, fun(Event, Acc) -> collector_string_representation (Event, Acc) end, "")
    ++
    "}\n"
    .

%%%%
% String representation of the collector's content

%% show state change
collector_string_representation ({event, _Priority, _Time1, _Time2, Sender, Sender,
        state_change,[State]}, Acc) ->
    Acc ++ io_lib:format("\"~s\" rbox \"~s\" [label=\"~s\"];\n",
        [convert_process_id (Sender),convert_process_id(Sender), State]);

%% name process
collector_string_representation ({event, _Priority, _Time1, _Time2, Sender, Sender,
        name_process,[Name]}, Acc) ->
    Acc ++ io_lib:format("\"~s\" box \"~s\" [label=\"~s\"];\n",
        [convert_process_id (Sender),convert_process_id(Sender), Name]);

%% call -> arrows
collector_string_representation ({event, _Priority, _Time1, _Time2, Sender, Receiver,
        Message, _More}, Acc) -> Acc ++ 
    io_lib:format("\"~s\" => \"~s\" [label=\"~w\"];\n", [convert_process_id(Sender), convert_process_id(Receiver), Message])
    .

%%%%
% iterate over a collector
iterate (Collector, Fun, Acc) ->
    et_collector:iterate (Collector, first, infinity, Fun, Acc).


%%%%
% the msc program doesn't like < and >
convert_process_id (Pid) ->
    lists:filter(fun(E) -> (E =/= $<) and (E =/= $>) 
        end,
        io_lib:write(Pid)).

ping (C) ->
    receive
        {ping, Sender} ->
            C ! {c_collect, {Sender, self(), ping}},
            Sender ! {pong, self()}
    end.


test () ->
    % create a collector
    Collector = start_collector (),
    % report the first events
    Collector ! {c_collect, {self(), self(), hello_world}},
    % ping process
    PingPid = spawn (collector, ping, [Collector]),
    PingPid ! {ping, self()},
    receive 
        {pong, Sender} ->
            Collector ! {c_collect, {Sender, self(), pong}}
    end,

    Collector ! {c_print},
    Collector ! {c_stop},
    ok.
