#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/gpb/ebin

-include("include/builtin_pb.hrl").

main([Host, PortStr]) ->
  Port = list_to_integer(PortStr),
	{ok, Sock} = gen_tcp:connect(Host, Port, 
                                 [binary, {packet, 0}, {active, false}]),
  {ok, #'PING_OUT'{str = "hello"}} = clnt_stub:call(builtin, builtinPing, #'PING_IN'{str = "hello"}, Sock);
main([]) ->
  usage().

usage() ->
  io:format("usage: client [port]~n"),
  halt(1).
