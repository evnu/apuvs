%causal ordered multicast
-module(com).
-export([test/0,init/0]).

%  the vector clock will be represented as a erlang dictionary, which maps a Pid to a
%  Clock


% after spawn, a com object will wait for a message which indicates it's group.
% the group list _MUST INCLUDE THE PID OF THIS PROCESS!_
init () ->
    receive
        % TODO check if Group is a list
        {group, Group} ->
            % for each Pid from the Group -> create {Pid, 0}
            % we need to do this to create a list from which we can build a dictionary/map
            TempGroup = lists:map(fun(Pid) -> {Pid,0} end, Group),
            TempDictionary = dict:from_list(TempGroup),
            Dictionary = dict:map(fun(_,_) -> 0 end, TempDictionary),
            life (Dictionary, [])
    end.

life (Vg, HoldbackQueue) ->
    % check HoldbackQueue if something must be delivered
    NewHoldbackQueue = lists:filter (fun(Holded) -> check_deliver (Vg, Holded) end, HoldbackQueue),
    % we have to remeber those messages, which can't be delivered now
    FilteredHoldbackqueue = lists:filter (fun(Holded) -> not (check_deliver (Vg, Holded)) end, HoldbackQueue),
    % for all elements in the NewHoldBackQueue - deliver! Delivering messages updates the
    % internal vector clock.
    VgUpdated = deliver (Vg, NewHoldbackQueue),

    % at this point: if a message in the HoldbackQueue is not already delivered, then this
    % process must first receive a new message to fullfill the causal ordering
    % requirement.
    receive
        % com_multicast indicates that this process is supposed to multicast a message
        {com_multicast, Group, Message} ->
            VgTemp = dict:update_counter(self (), 1, VgUpdated),
            bem:multicast(Group, {bem_multicast, VgTemp, self(), Message}),
            life(VgTemp, NewHoldbackQueue);
        % receive a multicast
        {bem_multicast, VgTemp, Sender, Message} ->
            % store it for later
            life (VgUpdated, lists:append([{VgTemp, Sender,
                            Message}],FilteredHoldbackqueue));
        kill ->
            true
    end.

% check wether a certain message must be delivered or not
check_deliver (Vg, {VgSender, Sender, _}) ->
    check_condition (dict:fetch(Sender, VgSender) == dict:fetch(Sender, Vg) + 1) and compareDicts (Vg, VgSender, Sender).

% does this already exist in erlang..? Why do those guys hate types :-/
check_condition (Condition) ->
    if Condition -> true;
        true -> false
    end.

compareDicts (Vlocal, Vremote, J) ->
    compareDicts (dict:fetch_keys(Vlocal), Vlocal, Vremote, J).

% for all key \in keys: if key != J => Vlocal[key] >= Vremote[key]
compareDicts (Keys, Vlocal, Vremote, J) ->
    % NOTE: http://marcuswelz.com/2009/03/04/less-or-equal-in-erlang/
    TempKeys = lists:delete (J, Keys),
    lists:all (fun (Key) -> dict:fetch(Key, Vremote) =< dict:fetch(Key, Vlocal) end,
        TempKeys).

% if no more messages to be send - return the new vector clock
deliver (Vg, []) -> 
    Vg;

% we don't need the VgSender in {VgSender, Sender, Message} anymore, as we already checked
% the clock in compareDicts
deliver(Vg, [{_, Sender, Message}|Tail]) ->
    % deliver it
    deliver (Sender, Message),
    VgUpdated = dict:update_counter (Sender, 1, Vg), % update local clock for sender
    deliver (VgUpdated, Tail);


deliver (Sender, Message) ->
    io:format("~w received ~w from ~w\n", [self (), Message, Sender]).

%%%%%%%%%%%%%%%%
%
% test run
%
%%%%%%%%%%%%%%%%

test () ->
    % spawn 5 processes
    % TODO shorten with erlang syntactic sugar
    Pid1 = spawn (com, init, []),
    Pid2 = spawn (com, init, []),
    Pid3 = spawn (com, init, []),
    Pid4 = spawn (com, init, []),
    Pid5 = spawn (com, init, []),
    Group = [Pid1, Pid2, Pid3, Pid4, Pid5],
    % initialize each process - tell it about it's group
    Pid1 ! {group, Group}, 
    Pid2 ! {group, Group}, 
    Pid3 ! {group, Group}, 
    Pid4 ! {group, Group}, 
    Pid5 ! {group, Group},

    % tell Pid1 to multicast hello_world and show that delivering is causaly ordered
    Pid1 ! {com_multicast, Group, hello_world},

    Pid1 ! {com_multicast, Group, 1},
    Pid2 ! {com_multicast, Group, 1},
    Pid1 ! {com_multicast, Group, 2},
    Pid2 ! {com_multicast, Group, 2},
    Pid1 ! {com_multicast, Group, 3},
    Pid2 ! {com_multicast, Group, 3},
    Pid1 ! {com_multicast, Group, 4},
    Pid2 ! {com_multicast, Group, 4},
    Pid1 ! {com_multicast, Group, 5},
    Pid2 ! {com_multicast, Group, 5},

    timer:sleep (10),
    % kill group
    lists:foreach(fun(Pid) -> Pid ! kill end, Group)
    .

