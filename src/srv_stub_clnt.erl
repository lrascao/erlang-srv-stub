-module(srv_stub_clnt).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-record(state, {
	parent,
	socket}).

start_link(Socket, Parent) ->
	gen_server:start_link(?MODULE, [Socket, Parent], []).

init([_SupervisorPid, [Socket, Parent]]) ->
	{ok, #state{parent = Parent, socket = Socket}}.

terminate(normal, _State) ->
	ok;
terminate(shutdown, _State) ->
	ok.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({tcp, Socket, Data}, State = #state{socket = Socket}) ->
	io:format("got data ~p on socket ~p, state socket ~p~n", [Data, Socket, State#state.socket]),
	{noreply, State};
handle_info({tcp_closed, Socket}, State = #state{socket = Socket}) ->
	{stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
