-module(main).
-export([start/0,init/0]).


% start()
%
% Compile the different files and start a new ring.
% Quentin Nater - 12/21
start() ->
	io:fwrite("........................................~n...Simple Interface for DHTs............~n...Starting the ring....................~n...Quentin Nater & Christopher Artero...~n...University of Fribourg / 2021........~n........................................~n"),
  
  c:c(node),
  c:c(key),
  c:c(ring),
  ring:start().

% init()
%
% Initialize a client
% Quentin Nater - 12/21
init() ->
	io:fwrite("........................................~n...Simple Interface for DHTs............~n...Initialization.......................~n...Quentin Nater & Christopher Artero...~n...University of Fribourg / 2021........~n........................................~n"),
  
  c:c(node),
  c:c(key),
  c:c(ring),
  ring:init().