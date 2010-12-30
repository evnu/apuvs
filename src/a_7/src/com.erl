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
    NewHoldbackqueue = dict:map (fun(Holded) -> check_deliver (Vg, Holded) end, HoldbackQueue),
    % for all elements in the NewHoldBackQueue - deliver!
    receive
        {com_multicast, Group, Message} ->
            % fetch(Key, Dict) -> Value
            VgTemp = dict:update_counter(self (), 1, Vg),
            bem:multicast(Group, {VgTemp, self(), Message}),
            life(VgTemp, NewHoldbackqueue);
        % receive a multicast
        {VgTemp, Message} ->
            % store it for later
            life (Vg, [{VgTemp, Message}|NewHoldbackqueue])
    end.

% check wether a certain message must be delivered or not
check_deliver (Vg, {VgSender, Sender, _}) ->
        dict:fetch(Sender, VgSender) == dict:fetch(Sender, Vg) + 1
        and compareDicts (Vg, VgSender, Sender) 
    .

compareDicts (Vlocal, Vremote, J) ->
    compareDicts (dict:fetch_keys(Vlocal), Vlocal, Vremote, J).

compareDicts (Keys, Vlocal, Vremote, J) ->
    % for all keys: if key != J => Vlocal >= Vremote
    % note: http://marcuswelz.com/2009/03/04/less-or-equal-in-erlang/
    TempKeys = lists:delete (J, Keys),
    lists:all (fun (Key) -> dict:fetch(Key, Vremote) =< dict:fetch(Key, Vlocal) end,
        TempKeys).

% compareDicts ([], _,_,_) -> true;
% 
% compareDicts ([Key|T], Vlocal, Vremote, J) ->
%     if Key /= J ->
%             if  (dict:fetch(Key, Vremote) > dict:fetch(Key, Vlocal)) -> false;
%                 true -> compareDicts (T, Vlocal, Vremote, J)
%             end
%             ;
%         true -> compareDicts (T, Vlocal, Vremote, J)
%     end.



