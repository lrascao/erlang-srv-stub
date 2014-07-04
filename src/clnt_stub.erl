%% coding: latin-1
%% @author Luis Rascão <luis.rascao@gmail.com>
%% @copyright 2014 Luis Rascão

%% @version 1.0.0
%% @title client stub
%% @doc client stub methods

-module(clnt_stub).

-export([call/4]).

call(Module, Proc, Input, Sock) ->

  % now build the protobuf module name
  ProtobufModule = list_to_atom(atom_to_list(Module)++"_pb"),
  % now, given the proc name we get it's input and output arguments
  RpcInfo = list_to_atom(atom_to_list(Proc)++"_info"),
  {RpcInputMessage, RpcOutputMessage} = ProtobufModule:RpcInfo(),
  % get the encode and decode input/output arguments
  EncodeInputMethod = list_to_atom("encode_"++atom_to_list(RpcInputMessage)),
  DecodeOutputMethod = list_to_atom("decode_"++atom_to_list(RpcOutputMessage)),

  % encode the input
  InputData = ProtobufModule:EncodeInputMethod(Input),

  {ok, Request} = srv_stub_framing:encode_protobuf_request(1, 0, atom_to_list(Module), atom_to_list(Proc), list_to_binary(InputData)),

  gen_tcp:send(Sock, Request),
  % get the header, which is two integers (4 bytes each)
  {ok, Header} = gen_tcp:recv(Sock, 8),
  % decode it, then read the rest
  {ok, 6006, PayloadSize, Rest} = srv_stub_framing:decode_header(Header),

  % we are to ready the actual payload data plus another two integers (4 bytes each)
  % they are the message id and flags
  {ok, Payload} = gen_tcp:recv(Sock, PayloadSize + 8),
  
  % extract all the relevant information
  {ok, MessageId, Flags, OutputData} = srv_stub_framing:decode_protobuf_reply(Payload),
  
  % decode the output
  Output = ProtobufModule:DecodeOutputMethod(OutputData),
  {ok, Output}.
