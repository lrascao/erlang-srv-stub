-module(builtin).

-export([builtinPing/1]).

-include("builtin_pb.hrl").

% result of builtinPing is simply the same string
builtinPing(#ping_in{str = Str}) ->
  #ping_out{str = Str}.
