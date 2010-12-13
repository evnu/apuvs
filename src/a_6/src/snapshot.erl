-module(snapshot).
-export([snapshot/2]).

%%%%%%%%
%
% snapshot
%
%  Channels are lists of processes

snapshot(OutgoingChannels, IncomingChannels, In) ->
    snapshot(OutgoingChannels, IncomingChannels, 1).

snapshot(OutgoingChannels, IncomingChannels, NumberOfMarkers) ->
    % send marker to each outgoing channel
    sendMarkerToOutgoing (OutgoingChannels),
    recordMessages (IncomingChannels, NumberOfMarkers, [])
.

% received marker on all incoming channels
recordMessages (IncomingChannels, NumberOfMarkers, ListOfSavedMessages) when length(IncomingChannels) == NumberOfMarkers -> 
    % resend ListOfSavedMessages to yourself
    resend (reverse (ListOfSavedMessages))
    ;

recordMessages (IncomingChannels, NumberOfMarkers, ListOfSavedMessages) ->
    receive
        {marker} -> recordMessages (IncomingChannels, NumberOfMarkers + 1,
                ListOfSavedMessages);
        Msg -> 
            % record message
            io:format ("Received ~w\n", [Msg]),
            recordMessages (IncomingChannels, NumberOfMarkers, [Msg | ListOfSavedMessages])
    end .

% we have to resend the saved messages to actually process them 
resend ([]) -> ;
resend ([H|T]) ->
    self () ! H,
    resend (T).



%%%%%%%%
%
% Send markers
%
sendMarkerToOutgoing ([]) -> .
sendMarkerToOutgoing ([H|T]) -> 
    H ! {marker},
    sendMarkerToOutgoing(T).

