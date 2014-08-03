-module(builtin).

-export([builtinPing/1]).

-include("builtin_pb.hrl").

% result of builtinPing is simply the same string
builtinPing(#'PING_IN'{str = Str}) ->
  #'PING_OUT'{str = Str}.
