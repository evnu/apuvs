-module(lamdy).
-export([run/4, distributor/3, buyer/3]).

distributor(_, Storage, _) when Storage < 10 ->
    io:format("DIST: I don't have enough screws....bye bye\n"),
    exit(empty);

distributor(Acc, Storage, false) ->
    receive
        {Number, Price} ->
            %send screws to buyer
            buy ! {Number},
            distributor(Acc + Price, Storage - Number, false);
				{snapshot} ->
					io:format ("Distributor <Konto: ~B , Schrauben: ~B>\n", [Acc, Storage]),
					distributor(Acc, Storage, true)
    end;
distributor(Acc, Storage, true) ->
		receive
				{Number, Price} ->
						io:format("Distributor: received ~B screws for ~B money\n", [Number, Price]),
						buy ! {Number},
						distributor(Acc + Price, Storage - Number, true);
				{snapshot} -> % finished
						distributor(Acc, Storage, false)
			end.

buyer(Acc, _, _) when Acc < 10 ->
    io:format("BUYER: Darn...i need a dollar..dollar..dollar is what i need\n"),
    exit(empty);

% no snapshot
buyer (Acc, Storage, false) ->
    distrib ! {10, 50},
    Newacc = Acc - 50,
    receive
        {Number} when is_integer(Number) ->
            buyer(Newacc, Storage + Number, false);
				{snapshot} ->
						io:format("Buyer <Konto: ~B , Schrauben: ~B>\n", [Acc, Storage]),
						distrib ! {snapshot},
						buyer(Newacc, Storage, true)
    end;

buyer(Acc, Storage, true) ->
		% record incoming messages
		receive
				{Number} when is_integer (Number) -> 
					io:format("Buyer - received ~B screws\n", [Number]),
					buyer(Acc, Storage + Number, true);
				{snapshot} -> % finished snapshot
					buyer(Acc, Storage, false)
		end.

run(Dacc, Dstore, Bacc, Bstore) ->
    Distributor = spawn(lamdy, distributor, [Dacc, Dstore, false]),
    register(distrib, Distributor),
    Buyer = spawn (lamdy, buyer, [Bacc, Bstore, false]),
    register(buy, Buyer),
    link(Distributor),
    link(Buyer),
		Buyer ! {snapshot}
		.
