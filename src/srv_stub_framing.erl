-module(srv_stub_framing).

-export([decode_header/1, 
         encode_protobuf_request/5, decode_protobuf_request/1, 
         encode_protobuf_reply/3, decode_protobuf_reply/1]).

strip(Input) -> 
  [ X || <<X>> <= Input, X /= 0].

pad(Input, Size, Padding) ->
  <<Input/binary, Padding:((Size - size(Input)) * 8)>>.

% Header consists of 
%     4 bytes - Protocol id
%     4 bytes - payload size
decode_header(<<Protocol:32, PayloadSize:32, Rest/binary>>) ->
  {ok, Protocol, PayloadSize, Rest};
decode_header(<<_InvalidHeader/binary>>) ->
	{error}.

encode_header(PayloadSize) ->
  Header = <<6006:32, PayloadSize:32>>,
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

  % xr marker consists of
	%		3 bytes: 'm','m','v'
  % message info consists of:
  %   4 bytes: message id
  %   4 bytes: is reply or not
  %   4 bytes: flags
  % proc info consists of:
  %   64 bytes: module name
  %   64 bytes: proc name
  %   4 bytes: proc id
decode_protobuf_request(<<"mmv", MessageId:32, IsReply:32, Flags:32, ModuleName:64/bytes, ProcName:64/bytes, ProcId:32, Data/binary>>) ->
  {ok, MessageId, IsReply, Flags, strip(ModuleName), strip(ProcName), ProcId, Data};
decode_protobuf_request(<<_InvalidRequest/binary>>) ->
	{error}.

encode_protobuf_reply(MessageId, Flags, OutputData) ->
  % get a valid header
  {ok, Header} = encode_header(size(OutputData)),
  Payload = <<Header/binary, MessageId:32, Flags:32, OutputData/binary>>,
  {ok, Payload}.

decode_protobuf_reply(Payload) ->
  <<MessageId:32, Flags:32, Data/binary>> = Payload,
  {ok, MessageId, Flags, Data}.
