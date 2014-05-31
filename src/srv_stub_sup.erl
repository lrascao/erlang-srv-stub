-module(srv_stub_sup).
-behaviour(supervisor).

% behaviour exports
-export([start_link/1, init/1]).
% others
-export([start_child/0]).

start_link(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

start_child() ->
	{ok, _Child} = supervisor:start_child(?MODULE, []).

init([Port]) ->
	% start listening on the provided Port
	{ok, Socket} = gen_tcp:listen(Port, [{packet, 0}, {active, once}]),
	% start the clients supervisor
	{ok, ClientsSupervisor}  = srv_stub_clnt_sup:start_link([]),

	% 
	spawn_link(fun start_listeners/0),
	% and return the supervision strategy
	{ok, {{simple_one_for_one, 60, 3600},
		 [{server,
		  {srv_stub, start_link, [Socket, ClientsSupervisor]},
		    permanent, _Shutdown = 1000, worker, [srv_stub]}
		 ]}}.

start_listeners() ->
	[start_child() || _ <- lists:seq(1, 5)],
	ok.
