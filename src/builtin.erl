-module(builtin).

-export([builtinPing/1]).

% result of builtinPing is simply the same string
builtinPing({ping_in, String}) ->
  {ping_out, String}.
