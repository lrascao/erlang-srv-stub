#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/gpb/ebin

main([Arg]) ->
  Port = list_to_integer(Arg),
  {ok, _} = srv_stub_sup:start_link(Port),
  io:format("listening on port ~p~n", [Port]),
  ok = timer:sleep(1000 * 60 * 60);  %% sleep a lot of time
main([]) ->
  usage().

usage() ->
  io:format("usage: start [port]~n"),
  halt(1).
