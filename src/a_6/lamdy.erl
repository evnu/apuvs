-module(lamdy).
-export([run/4, distributor/2, buyer/2]).

distributor(_, Storage) when Storage < 10 ->
    io:format("DIST: I don't have enough screws....bye bye\n"),
    exit(empty);

distributor(Acc, Storage) ->
    io:format("DIST: I have ~B screws and ~B creds.\n", [Storage, Acc]),
    receive
        {Number, Price} ->
            io:format("DIST: Received an order of ~B screws costing ~B creds\n",
                [Number, Price]),
            %send screws to buyer
            buy ! {Number},
            distributor(Acc + Price, Storage - Number)
    end.

buyer(Acc, _) when Acc < 10 ->
    io:format("BUYER: Darn...i need a dollar..dollar..dollar is what i need\n"),
    exit(empty);

buyer(Acc, Storage) ->
    io:format("BUYER: I have ~B screws and ~B creds.\n", [Storage, Acc]),
    distrib ! {10, 50},
    Newacc = Acc - 50,
    receive
        {Number} ->
            io:format("BUYER: Received ~B screws\n", [Number]),
            buyer(Newacc, Storage + Number)
    end.

run(Dacc, Dstore, Bacc, Bstore) ->
    Distributor = spawn(lamdy, distributor, [Dacc, Dstore]),
    register(distrib, Distributor),
    Buyer = spawn (lamdy, buyer, [Bacc, Bstore]),
    register(buy, Buyer),
    link(Distributor),
    link(Buyer).
