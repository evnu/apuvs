-module(bem).
-export([multicast/2]).

multicast(Group, Msg) -> pp2p_multicast(Group, Msg).

pp2p_multicast(Group, Msg) ->
    lists:foreach (fun(Receiver) -> Receiver ! Msg end, Group).
