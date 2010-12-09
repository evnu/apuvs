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
				{snapshot, Sender} -> % if the system wants us to do a snapshot, wait for other snapshot
            record_state ("Distributor", Acc, Storage),
            send_marker (buy),
            case Sender of
                system -> distributor(Acc, Storage, true);
                _ -> distributor(Acc, Storage, false)
            end
    end;
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
        {snapshot, Sender} -> % system tells us to do a snapshot
            record_state ("Buyer", Newacc, Storage),
            send_marker (distrib),
            case Sender of
                system -> buyer(Newacc, Storage, true);
                _  -> buyer(Newacc, Storage, false)
            end
    end;

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
		Buyer ! {snapshot, system}
		.
