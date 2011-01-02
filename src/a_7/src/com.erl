%causal ordered multicast
-module(com).
-export([spawnCom/0]).

%  the vector clock will be represented as a erlang dictionary, which maps a Pid to a
%  Clock


% after spawn, a com object will wait for a message which indicates it's group.
% the group list _MUST INCLUDE THE PID OF THIS PROCESS!_
spawnCom () ->
    init ().

init () ->
    receive
        {group, Group} ->
            TempDictionary = dict:from_list(Group),
            Dictionary = dict:map(fun(_,_) -> 0 end, TempDictionary),
            life (Dictionary, [])
    end.

life (Vg, HoldbackQueue) ->
    % check holdbackqueue if something must be delivered
    NewHoldbackQueue = lists:filter (fun(Holded) -> check_deliver (Vg, Holded) end, HoldbackQueue),
    % we have to remeber those messages, which can't be delivered now
    FilteredHoldbackqueue = lists:filter (fun(Holded) -> not (check_deliver (Vg, Holded)) end, HoldbackQueue),

    % for all elements in the NewHoldBackQueue - deliver!
    VgUpdated = deliver (Vg, NewHoldbackQueue),

    % at this point: if a message in the HoldbackQueue is not already delivered, then this
    % process must first receive a new message to fullfill the causal ordering
    % requirement.
    receive
        {com_multicast, Group, Message} ->
            % fetch(Key, Dict) -> Value
            VgTemp = dict:update_counter(self (), 1, VgUpdated),
            bem:multicast(Group, {bem_multicast, VgTemp, self(), Message}),
            life(VgTemp, NewHoldbackQueue);
        % receive a multicast
        {bem_multicast, VgTemp, Sender, Message} ->
            % store it for later
            life (VgUpdated, [FilteredHoldbackqueue|{VgTemp, Sender, Message}])
    end.

% check wether a certain message must be delivered or not
check_deliver (Vg, {VgSender, Sender, _}) ->
        dict:fetch(Sender, VgSender) == dict:fetch(Sender, Vg) + 1
        and compareDicts (Vg, VgSender, Sender).

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
    io:format("Received ~w from ~w\n", [Message, Sender]).

