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
    receive 
        {m_group, Group} -> ok
    end,
    Config = {ApplicationLayerPid, Group},
    life(Config, released, false, []).

life(Config = {ApplicationLayerPid, Group}, State, Voted, ReplyQueue) ->
    receive
        % we only accept the following two messages from our known upper layer
        {m_enter_cs, ApplicationLayerPid} ->
            _NewState = wanted,
            multicast (Group, {m_request, self()}),
            wait_for_request_replies(length(Group)),
            ApplicationLayerPid ! {a_ok, self()}, % tell application layer about enter cs
            life (Config, held, Voted, ReplyQueue)
            ;
        {m_exit_cs, ApplicationLayerPid} ->
            multicast (Group, {m_release, self()}),
            life(Config, released, Voted, ReplyQueue)
            ;
        {m_request, Sender} ->
            % TODO prettify
            case {State, Voted} of
                {held,_} ->
                    NewReplyQueue = lists:append(ReplyQueue, {m_ok, Sender}),
                    NewVoteState = false;
                {_,true} ->
                    NewReplyQueue = lists:append(ReplyQueue, {m_ok, Sender}),
                    NewVoteState = false;
                _ ->
                    send_ok (Sender),
                    NewVoteState = true,
                    NewReplyQueue = ReplyQueue
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
    Sender ! {ok, self()}.

wait_for_request_replies (0) -> ok;
wait_for_request_replies (Remaining) ->
    receive 
        {m_ok, _} ->
            wait_for_request_replies (Remaining - 1)
    end.

multicast (Group, Message) ->
    [Pid ! Message || Pid <- Group].
