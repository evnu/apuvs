-module(acceptor).
-export([initialize/1]).

%%%%%%%
% Initialize an acceptor
%

initialize(C) ->
    R_ack = 0,     %% last acknowledged round
    R_acc = 0,     %% last accepted round
    V     = null,  %% current local value
    % receive learner list from the creator
    LearnerList = 
    receive
        LL when is_list(LL) ->
           LL ;
        _ ->
            exit("Unexpected message in initialization.")
    end,
    life (C, R_ack, R_acc, V, LearnerList)
    . % END OF FUNCTION


%%%%%%%
% Main function of an acceptor process.
%   See initialize for comments on the parameters.
%
life (C, OldR_ack, OldR_acc, OldV, LearnerList) ->
    {NR_ack, NR_acc, NV} =
    receive 
        {{prepare,R_rcv},Sender}  -> 
            % R_rcv == the round of the proposer
            if ((R_rcv > OldR_ack) and (R_rcv > OldR_acc)) -> 
                    R_ack = R_rcv,
                    Sender ! {{ack,R_ack,OldV,OldR_acc},self()},
                    {R_rcv, %% update R_rcv
                        OldR_acc, 
                        OldV};
                true ->
                    {OldR_ack, OldR_acc, OldV}
            end;

        {{accepted,R,W}, _Sender} ->
            if ((R >= OldR_ack) and (R > OldR_acc)) -> 
                    % a proposer accepted by majority. Lets tell the learners and finish this.
                    R_acc = R,
                    V = W,
                    [X ! {{accepted,R_acc,V},self()}|| X <- LearnerList], % @Kai: kein , vor end
                    {OldR_acc, R_acc, V} % we updated the last accepted round 
                    ;

                true ->
                    % nothing is changed
                    {OldR_acc, OldR_acc, OldV} 
            end	;
        _ ->
            exit ("Received unexpected message.")
    end,
    % recurse
    life (C, NR_ack, NR_acc, NV, LearnerList)
    . % END OF FUNCTION

