%causal ordered multicast
-module(com).
-export([test/0,init/0]).

%%%%%%%%
% NOTE:
%  The vector clock will be represented as an erlang dictionary, which maps a Pid to a
%  Clockcounter


%%%%%%%
% Initialization.
% after spawn, a com object will wait for a message which tells it about its group.
% NOTE: 
%   The group list _MUST INCLUDE THE PID OF THIS PROCESS!_
init () ->
    receive
        {group, Group} ->
            % for each Pid from the Group -> create {Pid, 0}
            % we need to do this to create a list from which we can build a dictionary/map
            TempGroup = lists:map(fun(Pid) -> {Pid,0} end, Group),
            TempDictionary = dict:from_list(TempGroup),
            Dictionary = dict:map(fun(_,_) -> 0 end, TempDictionary),
            life (Dictionary, [])
    end.

%%%%%%%
%
% This is the main method of a process. It checks which messages should be delivered and
% receives all incoming messages.
%
life (Vg, HoldbackQueue) ->
    % check HoldbackQueue if something must be delivered
    DeliverFromHoldbackQueue = lists:filter (fun(Holded) -> check_deliver (Vg, Holded) end, HoldbackQueue),
    % we have to remeber those messages, which can't be delivered now
    FilteredHoldbackqueue = lists:filter (fun({_,Sender,_} = Holded) -> not (check_deliver (Vg, Holded)) and (not check_condition(Sender == self())) end, HoldbackQueue),
    % for all elements in the DeliverFromHoldbackQueue - deliver! Delivering messages updates the
    % internal vector clock.
    VgUpdated = deliver (Vg, DeliverFromHoldbackQueue), % this is the actual co_deliver

    % at this point: if a message in the HoldbackQueue is not already delivered, then this
    % process must first receive a new message to fullfill the causal ordering
    % requirement.
    receive
        % com_multicast indicates that this process is supposed to multicast a message
        {com_multicast, Group, Message} ->
            VgTemp = dict:update_counter(self (), 1, VgUpdated),
            % we can send a bem_deliver immediately, as we know that we use a perfect
            % link..
            bem:multicast(Group, {bem_deliver, VgTemp, self(), Message}),
            life(VgTemp, FilteredHoldbackqueue);
        % received a bem_deliver, now procede and try to co_deliver it later
        {bem_deliver, VgTemp, Sender, Message} ->
            % store it for later
            life (VgUpdated, lists:append([{VgTemp, Sender,
                            Message}],FilteredHoldbackqueue));
        % shut down this process cleanly
        kill ->
            % report error if message wasn't delivered
            case FilteredHoldbackqueue of
                [] -> 
                    io:format("~w will kill itself. It hasn't any undelivered messages.\n", [self()]);
                _ ->
                    io:format("~w will kill itself. It hasn undelivered messages.\n", [self()])
            end
    end.

%%%%%%%%
%
% Check conditions
% The following functions are used to check wether a certain message should be delivered
% or hold back.
%

% check wether a certain message must be delivered or not
check_deliver (Vg, {VgSender, Sender, _}) ->
    check_condition (dict:fetch(Sender, VgSender) == dict:fetch(Sender, Vg) + 1) and compareDicts (Vg, VgSender, Sender).


compareDicts (Vlocal, Vremote, J) ->
    compareDicts (dict:fetch_keys(Vlocal), Vlocal, Vremote, J).

% for all key \in keys: if key != J => Vlocal[key] >= Vremote[key]
compareDicts (Keys, Vlocal, Vremote, J) ->
    % NOTE: http://marcuswelz.com/2009/03/04/less-or-equal-in-erlang/
    TempKeys = lists:delete (J, Keys),
    lists:all (fun (Key) -> dict:fetch(Key, Vremote) =< dict:fetch(Key, Vlocal) end,
        TempKeys).

%%%%%%%%%%%%%%
% co_deliver
%

% if no more messages to be send - return the new vector clock
deliver (Vg, []) -> 
    Vg;

% we don't need the VgSender in {VgSender, Sender, Message} anymore, as we already checked
% the clock in compareDicts
deliver(Vg, [{_, Sender, Message}|Tail]) ->
    % deliver it
    deliver_message (Sender, Message),
    VgUpdated = dict:update_counter (Sender, 1, Vg), % update local clock for sender
    deliver (VgUpdated, Tail).

deliver_message (Sender, Message) ->
    io:format("~w received ~w from ~w\n", [self (), Message, Sender]).

%%%%%%%%
% Helper function - Introduce booleans.
% does this already exist in erlang..? Why do those guys hate types :-/
check_condition (Condition) ->
    if Condition -> true;
        true -> false
    end.

%%%%%%%%%%%%%%%%
%
% test run
%
%%%%%%%%%%%%%%%%

test () ->
    % spawn 5 processes
    Group = [Pid1, Pid2 | _] = lists:map(fun(_) -> spawn (com, init, []) end, lists:seq(1,5)),

    % initialize each process - tell it about it's group
    lists:foreach(fun(Pid) -> Pid ! {group, Group} end, Group),

    % tell Pid1 to multicast hello_world and show that delivering is causaly ordered
    Pid1 ! {com_multicast, Group, hello_world},

    % send the same message from alternating senders
    lists:foreach (fun(Num) -> Pid1 ! {com_multicast, Group, Num}, Pid2 ! {com_multicast, Group, Num} end, lists:seq (1,5)),

    % a final message
    Pid1 ! {com_multicast, Group, the_end},


    timer:sleep (10),
    % kill group
    lists:foreach(fun(Pid) -> Pid ! kill end, Group)
    .

