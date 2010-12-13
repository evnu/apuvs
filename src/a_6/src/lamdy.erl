-module(lamdy).
-export([run/4, distributor/3, buyer/3]).


%%%%%%%%%%%
%
% both distributor and buyer can use the following helper functions
%

record_state(Processname, Acc, Storage) -> 
					io:format ("~s <Konto: ~B , Schrauben: ~B>\n", [Processname, Acc, Storage]).
send_marker(Receiver) ->
          Receiver ! {snapshot, self()}.


%%%%%%%%%%%
%
% Distributor
%

distributor(_, Storage, _) when Storage < 10 ->
    io:format("DIST: I don't have enough screws....bye bye\n"),
    exit("distributor finishes");

distributor(Acc, Storage, false) ->
    receive
        {Number, Price} when is_integer(Number) and is_integer(Price) ->
            %send screws to buyer
            buy ! {Number},
            distributor(Acc + Price, Storage - Number, false);
				{snapshot, Sender} -> % received marker
            record_state ("Distributor", Acc, Storage),
            case catch send_marker (buy) of
                {'EXIT',_} ->
                    io:format ("Distributor lost connection to buyer\n"),
                    exit("Distributor finishes");
                _ ->
                    true
            end,
            case Sender of
                system -> distributor(Acc, Storage, true); % initiate snapshot
                _ -> distributor(Acc, Storage, false) % received message on all incoming channels
            end
    end;

% record incmoing messages
distributor(Acc, Storage, true) ->
		receive
				{Number, Price} when is_integer(Number) and is_integer(Price) ->
						io:format("Distributor: received ~B screws for ~B money\n", [Number, Price]),
						buy ! {Number},
						distributor(Acc + Price, Storage - Number, true);
				{snapshot, _} -> % finished - we can only be here if the initial message was from the system
						distributor(Acc, Storage, false)
			end.

%%%%%%%%%%%
%
% Buyer
%

buyer(Acc, _, _) when Acc < 10 ->
    io:format("BUYER: Darn...i need a dollar..dollar..dollar is what i need\n"),
    exit("Buyer finishes");

% no snapshot
buyer (Acc, Storage, false) ->
    distrib ! {10, 50},
    Newacc = Acc - 50,
    receive
        {Number} when is_integer(Number) ->
            buyer(Newacc, Storage + Number, false);
        {snapshot, Sender} -> % initiate snapshot 
            record_state ("Buyer", Newacc, Storage),
            case catch send_marker (distrib) of
                {'EXIT',_} ->
                    io:format ("Buyer lost connection to distributor\n"),
                    exit("Buyer finishes");
                _ ->
                    true
            end,

            case Sender of
                system -> buyer(Newacc, Storage, true);
                _  -> buyer(Newacc, Storage, false) % received message on all incoming channels
            end
    end;

% record incoming messages
buyer(Acc, Storage, true) ->
		% keep on sending messages
    distrib ! {10, 50},
    Newacc = Acc - 50,
		receive
			% record incoming messages
				{Number} when is_integer (Number) -> 
					io:format("Buyer - received ~B screws\n", [Number]),
					buyer(Newacc, Storage + Number, true);
				{snapshot, _} -> % finished snapshot
					buyer(Acc, Storage, false)
		end.

run(Dacc, Dstore, Bacc, Bstore) ->
    Distributor = spawn(lamdy, distributor, [Dacc, Dstore, false]),
    register(distrib, Distributor),
    Buyer = spawn (lamdy, buyer, [Bacc, Bstore, false]),
    register(buy, Buyer),
    link(Distributor),
    link(Buyer),
    % create a snapshot
		Buyer ! {snapshot, system}.
