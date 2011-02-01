-module(proposer).
-export([initialize/1]).

%%%%%%%
%
% Initialize a proposer
%

initialize (Collector) ->
    %% Get the list of acceptors
    receive
        Acceptors when is_list(Acceptors) ->
            Acceptors;
        _ ->
            Acceptors = unsafe,
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
    ],

    life (Configuration).


%%%%%%%
%
% Main function
%  This is the main function of a proposer.

life (Configuration) ->
    NewConf = receive
        {{propose, Value}, _Sender} ->
            send_new_proposal (Configuration, Value);
        {{ack, R_ack, V_i, R_i}, _Sender} ->
            R = proplists:get_value (r, Configuration),
            if R_ack == R ->
                    % R_ack == r
                    NewAckNum = proplists:get_value(acknum, Configuration) + 1,
                    R_latest  = proplists:get_value (r_latest, Configuration),
                    Majority  = proplists:get_value (majority, Configuration),

                    TempConf = 
                    if (R_i > R_latest) ->
                            % if the latest received round is older than the newly received round
                            % which was acknowledged by this acceptor, we know that we
                            % have to invalidate our round and reset latest_v (latest
                            % value) and r_latest (latest accepted round)
                            change_values([{r_latest, R_i}, {latest_v, V_i}], Configuration);
                        true -> Configuration
                    end,
                    if NewAckNum >= Majority ->
                            % If the proposer received more ACKs than needed for the
                            % majority, we'll declare that we accepted a value.
                            V_latest = proplists:get_value (latest_v, TempConf),
                            Value = 
                            if V_latest == null -> proplists:get_value(myvalue, TempConf,
                                    error_there_are_cases_where_we_didnt_decide_on_a_value);
                                true -> V_latest
                            end,
                            [Acceptor ! {accepted, proplists:get_value(r, TempConf), Value} || Acceptor <-
                                proplists:get_value(acceptors, TempConf)];
                        true -> true
                    end,
                    TempConf %% simply return the new configuration
                    ;
                true ->
                    % false
                    Configuration
            end

    end,
    life (NewConf) 
    . %% END OF FUNCTION

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
    [Acceptor ! {{prepare, proplists:get_value(r, NewConf)}, self()} || Acceptor <- proplists:get_value(acceptors, NewConf)],
        NewConf.

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

