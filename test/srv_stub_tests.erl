-module(srv_stub_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/builtin_pb.hrl").

srv_stub_ping_test() ->
	Port = 54321,
	{ok, _} = srv_stub_sup:start_link(Port),
	{ok, Sock} = gen_tcp:connect("localhost", Port, 
                                 [binary, {packet, 0}, {active, false}]),
	{ok, #'PING_OUT'{str = "hello"}} = clnt_stub:call(builtin, builtinPing, #'PING_IN'{str = "hello"}, Sock).

