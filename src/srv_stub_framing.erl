-module(srv_stub_framing).

-export([decode_header/1, 
         encode_protobuf_request/5, decode_protobuf_request/1, 
         encode_protobuf_reply/3, decode_protobuf_reply/1]).

strip(Input) -> 
  [ X || <<X>> <= Input, X /= 0].

pad(Input, Size, Padding) ->
  <<Input/binary, Padding:((Size - size(Input)) * 8)>>.

decode_header(Data) ->
  % Header consists of 
  %     4 bytes - Protocol id
  %     4 bytes - payload size
  <<Protocol:32, PayloadSize:32, Rest/binary>> = Data,
  {ok, Protocol, PayloadSize, Rest}.

encode_header(PayloadSize) ->
  Protocol = 6006,
  Header = <<Protocol:32, PayloadSize:32>>,
  {ok, Header}.
  
encode_protobuf_request(MessageId, Flags, ModuleName, ProcName, Data) ->
  % proc info consists of:
  %   64 bytes: module name
  %   64 bytes: proc name
  %   4 bytes: proc id
  ModuleNameBin = pad(list_to_binary(ModuleName), 64, 0),
  ProcNameBin = pad(list_to_binary(ProcName), 64, 0),
  ProcInfo = <<ModuleNameBin:64/bytes, ProcNameBin:64/bytes, 0:32, Data/binary>>,
  % message info consists of:
  %   4 bytes: message id
  %   4 bytes: is reply or not
  %   4 bytes: flags
  MessageInfo = <<MessageId:32, 0:32, Flags:32, ProcInfo/binary>>,
  % encode the xr marker
  Payload = <<"mmv", MessageInfo/binary>>,
  {ok, Header} = encode_header(size(Payload)),
  Request = <<Header/binary, Payload/binary>>,
  {ok, Request}.

decode_protobuf_request(Payload) ->
  % decode the xr marker
  <<"mmv", MessageInfo/binary>> = Payload,
  % message info consists of:
  %   4 bytes: message id
  %   4 bytes: is reply or not
  %   4 bytes: flags
  <<MessageId:32, IsReply:32, Flags:32, ProcInfo/binary>> = MessageInfo,
  % proc info consists of:
  %   64 bytes: module name
  %   64 bytes: proc name
  %   4 bytes: proc id
  <<ModuleName:64/bytes, ProcName:64/bytes, ProcId:32, Data/binary>> = ProcInfo,
  {ok, MessageId, IsReply, Flags, strip(ModuleName), strip(ProcName), ProcId, Data}.

encode_protobuf_reply(MessageId, Flags, OutputData) ->
  % get a valid header
  {ok, Header} = encode_header(size(OutputData)),
  Payload = <<Header/binary, MessageId:32, Flags:32, OutputData/binary>>,
  {ok, Payload}.

decode_protobuf_reply(Payload) ->
  <<MessageId:32, Flags:32, Data/binary>> = Payload,
  {ok, MessageId, Flags, Data}.
