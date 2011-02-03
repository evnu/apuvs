-module(proposer).
-export([initialize/1]).

%%%%%%%
%
% Initialize a proposer
%

initialize (Collector) ->
    %% Get the list of acceptors
    receive
        {Acceptors, _Sender} when is_list(Acceptors) ->
            Acceptors;
        _ ->
            Acceptors = unsafe,
            io:format("Unexpected message."),
            exit('Unknown value')
    end,
    Majority = length (Acceptors)/2 + 1,

    %% create a proplist to contain the configuration
    Configuration = [
         {r,0}           % local round number: begin with 0 because we add 1 anyways.
        ,{r_latest,0}    % number of the highest acknowledged round
        ,{latest_v,null} % value of the highest acknowledged round
        ,{acceptors, Acceptors}
        ,{majority, Majority}
        ,{acknum, 0}
        ,{collector, Collector}
        ,{timeout, 100}
    ],
    
    Collector ! {c_name_process, self(), "Proposer"},
    % tell collector about our state
    Collector ! {c_state_change, {self(), io_lib:format("r = ~w, r_latest = ~w,
                latest_v = ~w", [
                    proplists:get_value (r, Configuration),
                    proplists:get_value(r_latest, Configuration),
                    proplists:get_value(latest_v, Configuration)
                ])}},
    life (Configuration).


%%%%%%%
%
% Main function
%  This is the main function of a proposer.

life (Configuration) ->
    receive
        {{propose, V}, _sender} ->
            NewConf = send_new_proposal(Configuration, V),
            proposed(NewConf)
    end
    .

proposed(Configuration) ->
    R = proplists:get_value(r, Configuration),
    Timeout = proplists:get_value(timeout, Configuration),
    receive
        {{ack, R, Old_v, Old_r_ack}, _} ->
            io:format("Received ack-Message R: ~w OldV: ~w OldRAck: ~w\n",[R,
                    Old_v, Old_r_ack]),
            Acknum = proplists:get_value(acknum, Configuration),
            NewConf = change_value({acknum, Acknum + 1},
                change_mind(Configuration, Old_r_ack, Old_v)),
            Majority = proplists:get_value(majority, NewConf),
            io:format("Acknum: ~w Majority: ~w \n",[Acknum, Majority]),
            if
                Acknum >= Majority ->
                    Latest_v = propose_value(NewConf),
                    [Acceptor ! {{accept, R, Latest_v}, self()} || Acceptor <-
                        proplists:get_value(acceptors, NewConf)],
                    io:format("Sending accept-Message\n",[]),
                    life(NewConf);
                true ->
                    proposed(NewConf)
            end
    after Timeout ->
            Collector = proplists:get_value(collector, Configuration),
            Collector ! {c_state_change, {self(),
                    io_lib:format("Timeout...proposal ~w is bad",
                        [proplists:get_value(myvalue, Configuration)])}},
           life(Configuration)
   end
   .

propose_value(Conf) ->
    Latest_v = proplists:get_value(latest_v, Conf),
    if
        Latest_v =:= null ->
            Return_v = proplists:get_value(myvalue, Conf);
        true -> 
            Return_v = Latest_v
    end,
    Return_v
    .

%%%%%%%
% Send a new proposal
%  Send a new proposal sends prepare to all reachable acceptors. This function is also
%  responsible to choose a new round number for the proposer. We use a simple case here
%  and simply add 1 to the maximum of the last used round number and the last received
%  acknowledged round number.
%
send_new_proposal (Configuration, Value) ->
    % Note: proplist:delete doesn't fail, if the key to be deleted isn't found
    OldR = proplists:get_value (r, Configuration),
    LatestR = proplists:get_value(r_latest, Configuration),
    NewConf = change_values ([{acknum, 0}, {myvalue, Value}, {r, max(OldR, LatestR) + 1}], Configuration),
    % send prepare(r) to each acceptor
    io:format("Propose\n"),
    [Acceptor ! {{prepare, proplists:get_value(r, NewConf)}, self()} || Acceptor <- proplists:get_value(acceptors, NewConf)],
    NewConf
    .

change_mind(Conf, R, V) ->
    R_latest = proplists:get_value(r_latest, Conf),
    if
        R > R_latest ->
            NewConf = change_values([{r_latest, R}, {latest_v, V}], Conf);
        true -> NewConf = Conf
    end,
    NewConf
    .

%%%%%%%
% Change value in key store
%  Deletes the Key-Value pair and appends a new pair. Does nothing if the key doesn't
%  exit.
%
change_value (Property = {Key, _NewValue}, Configuration) ->
    [Property] ++ proplists:delete(Key, Configuration).


%%%%%%%
% Change list of values in key store
%  We want to ease the use of proplists. Give this function a list of properties, which
%  should be inserted into the key store. If the key of a given property is found, the
%  function will substitute the value with the new one.
%
change_values ([], Configuration) -> Configuration;
change_values ([H|T], Configuration) ->
    change_values (T, change_value(H, Configuration)).

max(A, B) ->
    Return = if
        A < B ->
            B;
        B < A ->
            A;
        true ->
            A
    end,
    Return
    .
