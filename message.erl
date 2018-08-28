-module(message).

%Two arguments in start: number of processes and number of rounds
-export([start/2, createProcess/3, passToken/0, controllToken/2]).

controllToken(0, [head|_]) ->
     head ! stop,
     done;

controllToken(N, Lst) ->
	[head | tail] = Lst,
    head ! {self(), token, tail},
    
    receive
		token ->
			io:fwrite("Round finished\n")
    end,		   
    controllToken(N - 1, Lst).
    

passToken() ->
	receive 
		stop ->
			io:fwrite("Stop received\n"),
    	    done;
		{controller, token, [head|[]]} ->		
		    controller ! token, 
		    passToken();
		{controller, token, [head|tail]} ->		
	        head ! {controller, token, tail},
	        passToken()
    end.

	    
createProcess(0, Lst, Rounds) -> 
    io:fwrite("~w~n", [Lst]),
    ok;

    createProcess(N, Lst, Rounds) when N > 1 ->
	io:fwrite("Hello\n"),
	Pid = spawn(message, passToken, []),
        createProcess(N-1, [Pid | Lst], Rounds);

    createProcess(N, Lst, Rounds) when N == 1 ->
	io:fwrite("Last controller\n"),
	spawn(message, controllToken, [Rounds, Lst]).
	

start(Threads, Rounds) ->
    createProcess(Threads, [], Rounds).

