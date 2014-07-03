-module(srv_stub_framing).

-export([decode_header/1, decode_protobuf_request/1, encode_protobuf_reply/3]).

strip(Input) -> 
  [ X || <<X>> <= Input, X /= 0].

decode_header(Data) ->
  % Header consists of 
  %     4 bytes - Protocol id
  %     4 bytes - payload size
  <<Protocol:32, PayloadSize:32, Rest/binary>> = Data,
  % obtain the payload
  <<Payload:PayloadSize/bytes>> = Rest,
  {ok, Protocol, Payload}.

encode_header(PayloadSize) ->
  Protocol = 6006,
  Header = <<Protocol:32, PayloadSize:32>>,
  {ok, Header}.
  
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
