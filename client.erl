#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/protobuffs/ebin

-include("include/builtin_pb.hrl").

main([Host, PortStr]) ->
  Port = list_to_integer(PortStr),
	{ok, Sock} = gen_tcp:connect(Host, Port, 
                                 [binary, {packet, 0}, {active, false}]),
  {ok, #ping_out{str = "hello"}} = clnt_stub:call(builtin, builtinPing, #ping_in{str = "hello"}, Sock);
main([]) ->
  usage().

usage() ->
  io:format("usage: client [port]~n"),
  halt(1).
