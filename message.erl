-module(message).
%Two arguments in start: number of processes and number of rounds
-export([bench/0, start/3, createProcess/4, passToken/0, controllerProcess/3]).

controllerProcess(0, [Head|_], BenchPid) ->
     Head ! stop,
     BenchPid ! done,
     done;

controllerProcess(N, Lst, BenchPid) ->
	[Head | Tail] = Lst,
	%io:fwrite("Controller arrived, Head = ~p, Tail = ~p\n", [Head, Tail]),
    Head ! {self(), token, Tail},

    receive
		token ->
			ok
			%io:fwrite("Round finished\n")
    end,
    controllerProcess(N - 1, Lst, BenchPid).


passToken() ->
	receive
		stop ->
			%io:fwrite("Stop received\n"),
    	    done;
		{Controller, token, []} ->
			%io:fwrite("Last in the list\n"),
		    Controller ! token,
		    passToken();
		{Controller, token, [Head|Tail]} ->
			%io:fwrite("Token received\n"),
	        Head ! {Controller, token, Tail},
	        passToken()
    end.


createProcess(N, Lst, Rounds, BenchPid) when N > 1 ->
	Pid = spawn(message, passToken, []),
        createProcess(N-1, [Pid | Lst], Rounds, BenchPid);

createProcess(N, Lst, Rounds, BenchPid) when N == 1 ->
	%io:format("~p~n", [Lst]),
	spawn(message, (controllerProcess), [Rounds, Lst, BenchPid]).


start(Threads, Rounds, BenchPid) ->
  createProcess(Threads, [], Rounds, BenchPid).

bench() ->
  Threads = 1000,
  Rounds = 1000,
  Before = os:perf_counter(1000),
  start(Threads, Rounds, self()),
  receive
    done ->
      After = os:perf_counter(1000),
      Elapsed = After - Before,
      io:format("~p~n", [Elapsed])
    end.
