-module(srv_stub).
-behaviour(gen_server).

% behaviour exports
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-export([start_link/2]).

-record(state, {
	socket,
	clients_supervisor,
	clients = []}).

-record(client, {
	socket,
	name,
	pid}).

start_link(Socket, ClientsSupervisor) ->
	gen_server:start_link(?MODULE, [Socket, ClientsSupervisor], []).

init([Socket, ClientsSupervisor]) ->
	gen_server:cast(self(), accept),
	{ok, #state{socket = Socket, clients_supervisor = ClientsSupervisor, clients = []}}.

terminate(normal, _State) ->
	ok;
terminate(shutdown, _State) ->
	ok.

handle_call(Request, _From, State) ->
	io:format("unknown call request received (~p)~n", [Request]),
	{noreply, State}.

handle_cast(accept, State = #state{socket = ListenSocket, clients = Clients}) ->
	% block, until a new client connects, then accept it
	{ok, Socket} = gen_tcp:accept(ListenSocket),

	% make the socket active so the data arrives through handle_info
	ok = inet:setopts(Socket, [{active, true}]),
	% start the only child of this supervisor, which is actually our Client
	{ok, ClientPid} = srv_stub_clnt_sup:start_child(State#state.clients_supervisor, [Socket, self()]),

	% create the new entry containing it's data
	NewClient = #client{socket = Socket, pid = ClientPid},

	io:format("new client started on socket ~p, pid ~p~n", [NewClient#client.socket, NewClient#client.pid]),
	% assign the newly accepted socket to the client which will be the controlling process
	ok = gen_tcp:controlling_process(Socket, NewClient#client.pid),

	% add the client to the list
	NewClients = [Clients | NewClient],
	% send an async accept message to ourselves in order to keep this worker accepting connections
	gen_server:cast(self(), accept),
	% return the new state containing the newly accepted client
	{noreply, State#state{clients = NewClients}};
handle_cast(Request, State) ->
	io:format("unknown cast request received (~p)~n", [Request]),
	{noreply, State}.

handle_info(Info, State) ->
	io:format("unknown info arrived: ~p~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
