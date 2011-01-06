-module(maekawa).
-export([initialization/1]).

%%%%%%%%
% API to maekawa process
% ApplicationLayer sends this process m_enter_cs to enter critical section and m_exit_cs
%   to release critical section
% The multicast layer sends this process m_request to indicate a critical section request
%   and m_release to indicate the release of a critical section
%
% TODO describe initialization of group

initialization (ApplicationLayerPid) ->
    Group = receive 
        {m_group, TupleGroup} -> 
            lists:map(fun({Pid,_}) -> Pid end, TupleGroup)
    end,
    Config = {ApplicationLayerPid, Group},
    io:format("~w\n",[Group]),
    life(Config, released, false, []).

life(Config = {ApplicationLayerPid, Group}, State, Voted, ReplyQueue) ->
    receive
        % we only accept the following two messages from our known upper layer
        {m_enter_cs, ApplicationLayerPid} ->
            io:format("~w {m_enter_cs, ~w}\n", [self(), ApplicationLayerPid]),
            _NewState = wanted,
            multicast:multicast (Group, {m_request, self()}),
            wait_for_request_replies(length(Group)),
            ApplicationLayerPid ! {a_ok, self()}, % tell application layer about enter cs
            life (Config, held, Voted, ReplyQueue)
            ;
        {m_exit_cs, ApplicationLayerPid} ->
            io:format("{m_exit_cs, ..}\n"),
            multicast:multicast (Group, {m_release, self()}),
            life(Config, released, Voted, ReplyQueue)
            ;
        {m_request, Sender} ->
            io:format("~w {m_request, ~w}\n", [self(), Sender]),
            % TODO prettify
            {NewVoteState, NewReplyQueue} = case {State, Voted} of
                {held,_} ->
                    {false, lists:append(ReplyQueue, {m_ok, Sender})};
                {_,true} ->
                    {false, lists:append(ReplyQueue, {m_ok, Sender})};
                _ ->
                    io:format("~w send_ok to ~w\n", [self(), Sender]),
                    send_ok (Sender),
                    {true, ReplyQueue}
            end,
            life(Config, State, NewVoteState, NewReplyQueue)
            ;
        {m_release, _Sender} ->
            case ReplyQueue of
                [] -> 
                    life (Config, State, false, []);
                [{Message, Receiver} | ReplyTail] ->
                    Receiver ! Message,
                    life (Config, State, true, ReplyTail);
                _ ->
                    exit({bad_arg})
            end
    end.

send_ok (Sender) ->
    Sender ! {m_ok, self()}.

wait_for_request_replies (0) -> ok;
wait_for_request_replies (Remaining) ->
    io:format("~w entering with ~B\n",[self(), Remaining]),
    SELF = self(),
    receive 
        {m_ok, _Sender} -> true;
        {m_request, SELF} -> true % m_request from self() == m_ok
    end,
    wait_for_request_replies (Remaining - 1).

