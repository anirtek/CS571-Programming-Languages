-module(horner2).
-export([server/1, client/2, loop/1, perform/2, append/2, calcAcc/1, auxCalcAcc/3, killAllProcesses/1]).
%Given Coeffs list [C[N], C[N-1], ..., C[1], C[0]], set up a server
%to evaluate the polynomial
%
%   C[N]*X^(N) + C[N-1]]*X^(N-1) + ... + C[1]*X^(1) + C[0]
%
%at some (yet unspecified) X.
%
%The server must consist of multiple Erlang processes with one Erlang 
%process per polynomial coefficient (there may be additional processes too).
%The processes for the coefficients should store their respective
%coefficients and communicate among themselves to evaluate the
%polynomial at a specified X using Horner's rule.
%
%The return value of this function should be the PID of the server.
%
%The details of the messages exchanged between the processes for
%evaluating the polynomial can be chosen by the implementation.
%However, the server must respond to a 'stop' message by stopping
%all the processes as well as itself.
%
%When a process for a coefficient is assisting in evaluating the
%polynomial, it must log to standard error it's stored coeffient
%and the value accumulated so far in the Horner evaluation.
%When a process for a coefficient is being stopped, it must
%log to standard error it's stored coefficient and the 'stop'
%message.  
server(Coeffs) -> 
	PidList = perform(Coeffs, []),
	ServerPid = spawn(horner2, loop, [PidList]),
	ServerPid.

loop(PidList) ->
       receive
	       {ClientPid, X} ->
		      Result =  auxCalcAcc(PidList, X, 0),
		      ClientPid ! {Result},
		      loop(PidList)
		      ;
	       stop ->
		       killAllProcesses(PidList)
       end.
killAllProcesses([]) ->
	true;
killAllProcesses([H|T]) ->
	H ! stop,
	killAllProcesses(T).

perform([], PidList) ->
	PidList ;
perform([H|T], PidList) ->
	Pid = spawn(horner2, calcAcc, [H]),
	perform(T, append( PidList,[Pid])).

append([H|T], Tail) ->
	[H|append(T, Tail)];
append([], Tail) ->
	Tail.

%Return the value at X of the polynomial stored within the polynomial server
%specified by PID.
client(Pid, X) ->
	Pid ! {self(), X},
	receive 
		{Result} ->
			Result
	end.


do_log(Coeff, Msg) ->
    io:format(standard_error, "coeff = ~w; ~w~n", [Coeff, Msg]).

calcAcc(C) ->
	receive
		{Pid, X, Acc} ->
			AccNew = (Acc*X)+C,
			do_log(C,AccNew),
			Pid ! {AccNew},
			calcAcc(C);
		stop ->
			do_log(C, stop)		
	end.

auxCalcAcc([], _, Acc) ->
	Acc;
auxCalcAcc([H|T], X, Acc) ->
	H ! {self(), X, Acc},
	receive
		{AccNew} ->
			auxCalcAcc(T, X, AccNew)
	end.

