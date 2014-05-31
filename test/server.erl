-module(server).
-export([start/1]).

start(Port) ->
	{ok, _} = srv_stub_sup:start_link(Port),
	ok.
