-module(srv_stub_clnt_protobuf).

-include("srv_stub.hrl").

-export([handle_request/1]).

handle_request(Request) ->
  % first step is to obtain the module name
  Module = list_to_atom(Request#request.module_name),
  % now build the protobuf module name
  ProtobufModule = list_to_atom(Request#request.module_name++"_pb"),
  % now, given the proc name we get it's input and output arguments
  RpcInfo = list_to_atom(Request#request.proc_name++"_info"),
  RpcName = list_to_atom(Request#request.proc_name),
  
  {RpcInputMessage, RpcOutputMessage} = ProtobufModule:RpcInfo(),
  %~ io:format("~s ~s:~s(~s)~n", [RpcOutputMessage, Module, Request#request.proc_name, RpcInputMessage]),
  % get the encode and decode input/output arguments
  DecodeInputMethod = list_to_atom("decode_"++atom_to_list(RpcInputMessage)),
  EncodeOutputMethod = list_to_atom("encode_"++atom_to_list(RpcOutputMessage)),
  % decode the input arguments
  DecodedInput = ProtobufModule:DecodeInputMethod(Request#request.data),  
  % finally invoke the method and get the result
  DecodedOutput = Module:RpcName(DecodedInput),
  
  io:format(" socket: ~p~n", [Request#request.socket]),
  io:format(" module name: ~s~n", [Request#request.module_name]),
  io:format(" proc name: ~s (#~p)~n", [Request#request.proc_name, Request#request.proc_id]),
  io:format(" input: ~p~n", [DecodedInput]),
  io:format(" output: ~p~n", [DecodedOutput]),
      
  %~ io:format("~s:~s(~p): ~p~n", [Request#request.module_name, Request#request.proc_name, DecodedInput, DecodedOutput]),
  %~ io:format("output: ~p, method: ~p~n", [DecodedOutput, EncodeOutputMethod]),
  % now encode the output to be returned
  EncodedOutput = ProtobufModule:EncodeOutputMethod(DecodedOutput),
  %~ io:format("output: ~p~n", [list_to_binary(EncodedOutput)]),
  {ok, list_to_binary(EncodedOutput)}.
