-module(srv_stub_clnt).
-behaviour(gen_server).

-include("srv_stub.hrl").
-include("srv_stub_protocol.hrl").

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

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({new_request, Request}, State) ->
  % take care of the protobuf request
  {ok, OutputData} = srv_stub_clnt_protobuf:handle_request(Request),
  % now frame the reply to be sent
  {ok, Reply} = srv_stub_framing:encode_protobuf_reply(Request#request.message_id, Request#request.flags, OutputData),
  % now write the reply to the socket client
  ok = gen_tcp:send(Request#request.socket, Reply),
	{noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_info({tcp, Socket, RawData}, State = #state{socket = Socket}) ->
	% decode the header, only protobuf is accepted
  {ok, 6006, PayloadSize, Rest} = srv_stub_framing:decode_header(list_to_binary(RawData)),
  % obtain the payload
  <<Payload:PayloadSize/bytes>> = Rest,
  % now in the possession of the payload decode it also
  % extract all the relevant information
  {ok, MessageId, IsReply, Flags, ModuleName, ProcName, ProcId, Data} = srv_stub_framing:decode_protobuf_request(Payload),
  % do a async message to self
  gen_server:cast(self(), {new_request, #request{
                                            socket = Socket,
                                            message_id = MessageId,
                                            is_reply = IsReply,
                                            flags = Flags,
                                            module_name = ModuleName,
                                            proc_name = ProcName,
                                            proc_id = ProcId,
                                            data = Data}}),
	{noreply, State};
handle_info({tcp_closed, Socket}, State = #state{socket = Socket}) ->
	io:format("client dropped on socket ~p~n", [Socket]),
	{stop, normal, State}.
