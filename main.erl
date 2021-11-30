-module(main).
-export([start/0]).

start() ->
	io:fwrite("........................................~n...Simple Interface for DHTs............~n...Quentin Nater & Christopher Artero...~n...University of Fribourg / 2021........~n........................................~n"),
  
  c:c(node),
  c:c(key),
  c:c(ring),
  ring:start().