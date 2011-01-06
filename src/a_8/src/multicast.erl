-module(multicast).
-export([multicast/2]).

multicast (Group, Message) ->
    [Pid ! Message || Pid <- Group].
