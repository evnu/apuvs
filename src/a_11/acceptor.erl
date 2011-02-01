-module(acceptor).
-export([initialize/1]).

%%%%%%%
%
% Initialize an acceptor
%

initialize(C) -> 
	MyPid = Pid,
	R_ack = 0,
	R_acc = 0,
	V = null.

receive 
	{{prep,R_rcv},Sender}  -> 
		if R_rcv > R_ack, R_rcv > R_acc -> 
			R_ack = R_rcv,
			Sender ! {{Sender,{ack,R_ack,V,R_acc}},self()},
		end. 
			
	{{acced,R,W}, Sender} ->
		if R >= R_ack, R > R_acc -> 
			R_acc = R,
			V = W,
			[X ! {{acced,R_acc,V},self()}|| X <- LearnerList],
		end.	
			
end.


