-module(snapshot).
-export([snapshot/2]).

%%%%%%%%
%
% snapshot
%
%  Channels are lists of processes

snapshot(OutgoingChannels, IncomingChannels) ->
    snapshot(OutgoingChannels, IncomingChannels, 1).

snapshot(OutgoingChannels, IncomingChannels, NumberOfMarkers) ->
    % send marker to each outgoing channel
    sendMarkerToOutgoing (OutgoingChannels),
    recordMessages (IncomingChannels, NumberOfMarkers, [])
.

% received marker on all incoming channels
recordMessages (IncomingChannels, NumberOfMarkers, ListOfSavedMessages) when length(IncomingChannels) == NumberOfMarkers -> 
    lists:reverse (ListOfSavedMessages)
    ;

recordMessages (IncomingChannels, NumberOfMarkers, ListOfSavedMessages) ->
    receive
        {marker} -> 
            recordMessages (IncomingChannels, NumberOfMarkers + 1, ListOfSavedMessages);
        Msg -> 
            % record message
            io:format ("Recorded ~w\n", [Msg]),
            recordMessages (IncomingChannels, NumberOfMarkers, [Msg | ListOfSavedMessages])
    end .

%%%%%%%%
%
% Send markers
%
sendMarkerToOutgoing ([]) -> true;
sendMarkerToOutgoing ([H|T]) -> 
    case catch H ! {marker} of
            {'EXIT',_} ->
                io:format ("Lost my connection to ~w. Aborting.\n", [H]);
            _ -> true
    end,
    sendMarkerToOutgoing(T).

