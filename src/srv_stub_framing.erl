-module(srv_stub_framing).

-export([decode_header/1, 
         encode_protobuf_request/5, decode_protobuf_request/1, 
         encode_protobuf_reply/3, decode_protobuf_reply/1]).

% remove all zero values from end of binary
strip(Input) -> 
  [ X || <<X>> <= Input, X /= 0].

% pad end of binary with Size occurrences of supplied value
pad(Input, Size, Padding) ->
  <<Input/binary, Padding:((Size - size(Input)) * 8)>>.

% header consists of 
%     4 bytes - Protocol id
%     4 bytes - payload size
decode_header(<<6006:32, PayloadSize:32, Rest/binary>>) ->
  {ok, PayloadSize, Rest};
decode_header(<<_InvalidHeader/binary>>) ->
	{error}.

% 
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
	% final result is header and payload contactenated
  {ok, <<Header/binary, Payload/binary>>}.

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

encode_protobuf_reply(MessageId, Flags, Data) ->
  % get a valid header containing the data size
  {ok, Header} = encode_header(size(Data)),
	% build a reply containing the header, message id, flags and the data
  {ok, <<Header/binary, MessageId:32, Flags:32, Data/binary>>}.

decode_protobuf_reply(<<MessageId:32, Flags:32, Data/binary>>) ->
  {ok, MessageId, Flags, Data};
decode_protobuf_reply(<<_InvalidaReply/binary>>) ->
	{error}.
