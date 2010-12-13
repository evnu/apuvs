-module(lamdy).
-export([run/0, run/4, distributor/2, buyer/2]).

%%%%%%%%%%%
%
% example
% - call lamdy:run() to start the example
%
% Note: the distributor initiates the snapshot if his message box is empty.
%

%%%%%%%%%%%
%
% Distributor
%

distributor(_, Storage) when Storage < 10 ->
    io:format("DIST: I don't have enough screws....bye bye\n"),
    exit("distributor finishes");

distributor(Acc, Storage) ->
    receive
        {Number, Price} when is_integer(Number) and is_integer(Price) ->
            %send screws to buyer
            buy ! {Number},
            distributor(Acc + Price, Storage - Number);
        {marker} ->
            io:format("DIST: Account: ~B \t\t Storage: ~B\n", [Acc, Storage]),
            UnprocessedMessages = snapshot:snapshot([buy], [buy]),
            distributor(Acc, Storage, UnprocessedMessages)
    after
        % if no message is buffered, do a snapshot
        0 -> io:format ("Distributor: Account: ~B\t\t Storage: ~B\n",[Acc, Storage]),
             snapshot:snapshot([buy], [distrib, buy]),
             io:format("\n"),
             distributor (Acc, Storage)
    end.

% Handle all messages which where left unprocessed in the snapshort algorithm
distributor (Acc,Storage,[]) ->
    distributor (Acc, Storage);

distributor (Acc,Storage, [{Money, Order}|T]) ->
    distributor (Acc + Money, Storage - Order, T).

%%%%%%%%%%%
%
% Buyer
%

buyer(Acc, _) when Acc < 10 ->
    io:format("BUYER: Darn...i need a dollar..dollar..dollar is what i need\n"),
    exit("Buyer finishes");

% no snapshot
buyer (Acc, Storage) ->
    distrib ! {10, 50},
    Newacc = Acc - 50,
    receive
        {Number} when is_integer(Number) ->
            buyer(Newacc, Storage + Number);
        {marker} ->
            io:format("BUYER: Account: ~B \t\t Storage: ~B\n", [Newacc, Storage]),
            UnprocessedMessages = snapshot:snapshot([distrib], [distrib]),
            buyer(Newacc, Storage, UnprocessedMessages)
    end.

% Handle all messages which where left unprocessed in the snapshort algorithm
buyer (Acc, Storage, []) ->
    buyer (Acc, Storage);

buyer (Acc, Storage, [{H}|T]) ->
    % we know how head looks as we know the structure of the messages
    buyer (Acc, Storage+H, T).


%%%%%%%%%%%
%
% Start methods
% 
%
run () ->
    run (1000,1000,1000,1000).

run(Dacc, Dstore, Bacc, Bstore) ->
    Distributor = spawn(lamdy, distributor, [Dacc, Dstore]),
    Buyer = spawn (lamdy, buyer, [Bacc, Bstore]),
    link(Distributor),
    link(Buyer),
    register(distrib, Distributor),
    register(buy, Buyer)
    .

