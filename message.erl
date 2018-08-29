-module(message).

%Two arguments in start: number of processes and number of rounds
-export([start/2, createProcess/3, passToken/0, controllerProcess/2]).

controllerProcess(0, [Head|_]) ->
     Head ! stop,
     done;

controllerProcess(N, Lst) ->
	[Head | Tail] = Lst,
	io:fwrite("Controller arrived, Head = ~p, Tail = ~p\n", [Head, Tail]),
    Head ! {self(), token, Tail},
    
    receive
		token ->
			io:fwrite("Round finished\n")
    end,		   
    controllerProcess(N - 1, Lst).
    

passToken() ->
	receive 
		stop ->
			io:fwrite("Stop received\n"),
    	    done;
		{Controller, token, []} ->
			io:fwrite("Last in the list\n"),		
		    Controller ! token, 
		    passToken();
		{Controller, token, [Head|Tail]} ->	
			io:fwrite("Token received\n"),	
	        Head ! {Controller, token, Tail},
	        passToken()
    end.

	    
createProcess(N, Lst, Rounds) when N > 1 ->
	Pid = spawn(message, passToken, []),
        createProcess(N-1, [Pid | Lst], Rounds);

createProcess(N, Lst, Rounds) when N == 1 ->
	io:format("~p~n", [Lst]),
	spawn(message, controllerProcess, [Rounds, Lst]).
	

start(Threads, Rounds) ->
    createProcess(Threads, [], Rounds).

