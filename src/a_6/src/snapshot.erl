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
    % resend ListOfSavedMessages to yourself
    %resend (lists:reverse (ListOfSavedMessages))
    io:format("done recording\n")
    ;

recordMessages (IncomingChannels, NumberOfMarkers, ListOfSavedMessages) ->
    receive
        {marker} -> recordMessages (IncomingChannels, NumberOfMarkers + 1,
                ListOfSavedMessages),
            io:format("recorded marker\n");
        Msg -> 
            % record message
            io:format ("Received ~w\n", [Msg]),
            recordMessages (IncomingChannels, NumberOfMarkers, [Msg | ListOfSavedMessages])
    end .

% we have to resend the saved messages to actually process them 
resend ([]) -> true;
resend ([H|T]) ->
    self () ! H,
    resend (T).



%%%%%%%%
%
% Send markers
%
sendMarkerToOutgoing ([]) -> true;
sendMarkerToOutgoing ([H|T]) -> 
    io:format("Sending marker ~w\n", [H]),
    H ! {marker},
    sendMarkerToOutgoing(T).

