-module(srv_stub_clnt_sup).
-behaviour(supervisor).

% behaviour methods
-export([start_link/1, init/1]).

-export([start_child/2]).

start_link([]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Socket, Parent) ->
	supervisor:start_child(?MODULE, [Socket, Parent]).

init([]) ->
	{ok, {{simple_one_for_one, 60, 3600},
		 [{client,
		  {srv_stub_clnt, start_link, []},
		    transient, 1000, worker, [srv_stub_clnt]}
		 ]}}.
