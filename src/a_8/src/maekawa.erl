-module(maekawa).
-export([initialization/2]).

%%%%%%%%
% API to maekawa process
%  initialization (Collector, ApplicationLayerPid) - initialize a maekawa process. See below
% For all other incoming messages: see comment above life (...)

% NOTE
% The maekawa module assumes that the Application Layer above can make use of the
% following message: %   + {a_ok, self()}, % tell application layer that it is allowed to enter critical section
% There is now message to indicate to the application layer that it released the critical
% section correctly. This is consistent with the given maekawa algorithm.


%%%%%%%%%
% Initialization
% The maekawa process must know it's overlaying Application Layer as well as it's consent
% group. The ID of the Application Layer is passed as the first argument to the following
% function. The list of processes in the process group must be sent afterwards by a
% message {m_group, ListOfPids}, where each element of ListOfPids is the list of pids of
% each process in the consent group.
% NOTE: The list ListOfPids _must_ contain the Pid of the receiving process!
%
initialization (Collector, ApplicationLayerPid) ->
    Collector ! {c_name_process, {self(), io_lib:format("Maekawa Layer below ~s",[collector:convert_process_id(ApplicationLayerPid)])}},
    % initial state
    Collector ! {c_state_change, {self(), state_string(released, false)}},
    Group = receive 
        {m_group, TupleGroup} -> 
            lists:map(fun({Pid,_}) -> Pid end, TupleGroup)
    end,
    Config = {ApplicationLayerPid, Group, Collector},
    io:format("~w\n",[Group]),
    life(Config, released, false, []).

%%%%%%%%%
% Main method of a maekawa process
% A maekawa process is basically a finite state machine. Therefore, it only reacts to
% incoming messages. The life function receives the following messages:
%  {m_enter_cs, ApplicationLayerPid} - the application layer wants to access a critical
%    section
%  {m_exit_cs, ApplicationLayerPid} - the application layer wants to leave a critical
%    section
%       NOTE: We don't do any error checking here! If the process isn't in a critical
%       section, undefined behaviour may occur.
%  {m_request, Sender} - Either this or another process wants to access a critical section
%  {m_release, Sender} - Either this or another process wants to release a critical
%    section
life(Config = {ApplicationLayerPid, Group, Collector}, State, Voted, ReplyQueue) ->
    receive
        % we only accept the following two messages from our known upper layer
        {m_enter_cs, ApplicationLayerPid} ->
            % tell collector that the application layer send us a request
            Collector ! {c_collect, {ApplicationLayerPid, self(), m_enter_cs}},
            io:format("~w {m_enter_cs, ~w}\n", [self(), ApplicationLayerPid]),
            _NewState = wanted,
            Collector ! {c_state_change, {self(), state_string (wanted, Voted)}},
            multicast:multicast (Group, {m_request, self()}),
            wait_for_request_replies(Collector, length(Group)),
            Collector ! {c_state_change, {self(), state_string (held, Voted)}},
            ApplicationLayerPid ! {a_ok, self()}, % tell application layer about enter cs
            life (Config, held, Voted, ReplyQueue)
            ;
        {m_exit_cs, ApplicationLayerPid} ->
            Collector ! {c_collect, {ApplicationLayerPid, self(), m_exit_cs}},
            io:format("{m_exit_cs, ..}\n"),
            multicast:multicast (Group, {m_release, self()}),
            Collector ! {c_state_change, {self(), state_string (released, Voted)}},
            life(Config, released, Voted, ReplyQueue)
            ;
        {m_request, Sender} ->
            Collector ! {c_collect, {Sender, self(), m_request}},
            io:format("~w {m_request, ~w}\n", [self(), Sender]),
            {NewVoteState, NewReplyQueue} = case {State, Voted} of
                {held,_} ->
                    Collector ! {c_state_change, {self(), state_string (State, false)}},
                    {false, lists:append(ReplyQueue, {m_ok, Sender})};
                {_,true} ->
                    Collector ! {c_state_change, {self(), state_string (State, false)}},
                    {false, lists:append(ReplyQueue, {m_ok, Sender})};
                _ ->
                    io:format("~w send_ok to ~w\n", [self(), Sender]),
                    send_ok (Sender),
                    Collector ! {c_state_change, {self(), state_string (State, true)}},
                    {true, ReplyQueue}
            end,
            life(Config, State, NewVoteState, NewReplyQueue)
            ;
        {m_release, Sender} ->
            Collector ! {c_collect, {Sender, self(), m_release}},
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

%%%%%%%%%
% As soon as a maekawa process tries to access a critical section, it has to wait for
% incoming messages from all other processes (including itself)
%
wait_for_request_replies (_, 0) -> ok;
wait_for_request_replies (Collector, Remaining) ->
    io:format("~w entering wait with ~B\n",[self(), Remaining]),
    SELF = self(), % why is this necessary :( useless useless useless useless
    receive 
        {m_ok, Sender} -> 
            Collector ! {c_collect, {Sender, self(), m_ok}};
        {m_request, SELF} -> 
            Collector ! {c_collect, {self(), self(), m_request}}
    end,
    wait_for_request_replies (Collector, Remaining - 1).


%%%%%%%%%
% Represent the current state as a string
% 
state_string (State, Voted) ->
    io_lib:format("{state=~w, voted=~w}",[State, Voted]).


