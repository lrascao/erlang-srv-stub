%% -*- erlang -*-
%%! -pa ./srv-stub-1.0.0/ebin

main([Arg]) ->
  Port = list_to_integer(Arg),
  ok = server:start(Port),
  io:format("listening on port ~p~n", [Port]),
  ok = timer:sleep(1000 * 60 * 60);  %% sleep a lot of time
main([]) ->
  usage().
  
usage() ->
  io:format("usage: start [port]~n"),
  halt(1).
