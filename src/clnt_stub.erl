%% coding: latin-1
%% @author Luis Rascão <luis.rascao@gmail.com>
%% @copyright 2014 Luis Rascão

%% @version 1.0.0
%% @title client stub
%% @doc client stub methods

-module(clnt_stub).

-include("gpb.hrl").

-export([call/4]).

call(Module, Proc, Input, Sock) ->

  % now build the protobuf module name, it is suffixed with _pb
  ProtobufModule = list_to_atom(atom_to_list(Module)++"_pb"),
  % now, given the proc name we get it's input and output arguments
  RpcDef = ProtobufModule:fetch_rpc_def(Proc),

  % encode the input
  InputData = ProtobufModule:encode_msg(Input),

  {ok, Request} = srv_stub_framing:encode_protobuf_request(1, 0, atom_to_list(Module), atom_to_list(Proc), InputData),

  gen_tcp:send(Sock, Request),
  % get the header, which is two integers (4 bytes each)
  {ok, Header} = gen_tcp:recv(Sock, 8),
  % decode it, then read the rest
  {ok, PayloadSize, _Rest} = srv_stub_framing:decode_header(Header),

  % we are to ready the actual payload data plus another two integers (4 bytes each)
  % they are the message id and flags
  {ok, Payload} = gen_tcp:recv(Sock, PayloadSize + 8),
  
  % extract all the relevant information
  {ok, _MessageId, _Flags, OutputData} = srv_stub_framing:decode_protobuf_reply(Payload),
  
  % decode the output
  Output = ProtobufModule:decode_msg(OutputData, RpcDef#rpc.output),
  {ok, Output}.
