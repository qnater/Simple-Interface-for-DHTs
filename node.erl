-module(node).
-export([launch/1]).

launch(N) ->
  io:format("~nStarting '~w' nodes...~n", [N]).