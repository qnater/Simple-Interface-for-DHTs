-module(ring).
-export([start/0, startRing/1, startRing/3, stop/1, listener/2, handling/1, generateKey/1, generateKey/0, getInfo/2, receiver/0, init/0, login/2, login/3, lookFor/3, postKey/3, postKey/4]).

-define(VERBOSITY, 0).
-define(Mbit,64).
-define(TimeToStabilize, 20000).

%Timeout
-define(Timeout, 10000).

% start()
%
% use to start and build a new ring
% Quentin Nater - 12/21
start() ->
    timer:start(),
    register(myring, self()),  % save the main process
    io:format(".......Main Process : ~w...........~n.......Size of the ring : ~w............~n........................................~n~n", [self(),?Mbit]),
    startRing(4).

% startRing(N)
% N : Number of process
%
% use to start and build a new ring of the first N processes
% Quentin Nater - 12/21
startRing(N) ->
    io:format("....INITIALIZATION........................................~n"),
    Key = generateKey(),
    StarterPid  = node:deploy(Key, nil, []),
    io:format("..startRing (StarterPid) : ~w ~w....................~n", [Key, StarterPid]),
    startRing(N-1, StarterPid, [{Key, StarterPid}]).


% startRing(N)
% N     : Number of process
% Spid  : Reference process in the ring
% Nodes : Array of all processes (nodes) inside the ring
%
% use to start and build a new ring of N processes
% Quentin Nater - 12/21
startRing(N,Spid,Nodes) when N > 0 ->
    Key = generateKey(Nodes),
    Pid = node:deploy(Key, Spid, []),
    io:format("..startRing (StarterPid) : ~w ~w....................~n", [Key, Pid]),
    timer:sleep(1000),
    startRing(N-1,Spid,[{Key,Pid}|Nodes]);
    
  
% startRing(_,_,Nodes)
% _     : last process
% _     : last process in the ring
% Nodes : Array of all processes (nodes) inside the ring
%
% use to build and test the created ring
% Quentin Nater - 12/21
startRing(_,_,Nodes) -> 

   io:format("~n....TEST/INTERACTION......................................~n"),


  % DYNAMIC INSERT OF KEY...............................
  myring ! {h,addkey,9265,nil},
  myring ! {h,addkey,3596,nil},

  % DYMAMIC INSTER OF NODE.............................
  myring ! {h,addnode,nil},

  
  % LOGS...............................................
  myring ! {info,nil},

  % LISTENER...........................................
  handling(Nodes),

  Nodes.

% handling(Nodes)
% Nodes : Array of all processes (nodes) inside the ring
%
% use to handle all messages inside the main process
% Quentin Nater - 12/21
handling(Nodes) ->
  {_,Process} = lists:last(Nodes), % reference
  timer:sleep(500),
  io:format("..wait........................~n", []),
  
  receive
    {h, lookfor, Key, Client} -> % look for a key
      node:getEntry(Key, Process, myring),
      listener(Key, Client),
      {myring,Client} ! {lookkeys, Key}, % inform the client
      timer:sleep(1200),
      handling(Nodes);

     {h, addkey, Value, Client} -> % add a new key and generate a new ID
      Key = generateKey(Nodes),
      node:addEntry(Key, Value, Process, myring),
      listener(Key, Client),
      {myring,Client} ! {get, keys, Key}, % inform the client
      timer:sleep(200),
      handling(Nodes);

   
    {h, addkey, Key, Value, Client} -> % add a new key
      node:addEntry(Key, Value, Process, myring),
      listener(Key, Client),
      {myring,Client} ! {get, keys, Key}, % inform the client
      timer:sleep(200),
      handling(Nodes);

  
    {h, addnode, Client} -> % add a new node and generate a new ID
      Key = generateKey(Nodes),
      Pid = node:deploy(Key, Process, []),
      UpdatedNodes = [{Key,Pid}|Nodes], % update the current array of nodes
      timer:sleep(1500),
      {myring,Client} ! {getnodes, UpdatedNodes}, % inform the client
      handling(UpdatedNodes);

    {h, addnode, Key, Client} -> % add a new node
      Pid = node:deploy(Key, Process, []),
      UpdatedNodes = [{Key,Pid}|Nodes], % update the current array of nodes
      timer:sleep(1500),
      {myring,Client} ! {getnodes, UpdatedNodes}, % inform the client
      handling(UpdatedNodes);

    {info, Client} -> % display all information about the ring
      [Y!logs || {_,Y} <- Nodes], % ask all nodes for logs
      {myring,Client} ! {get, Nodes},  % inform the client
      io:format(">> Nodes : ~w ~n", [Nodes]),
      timer:sleep(200),
      handling(Nodes)
  end.


% listener(Key, Client)
% Key     : Current key which we work on
% Client  : Client who asks the information
%
% receiver to handle the response from the nodes/client
% Quentin Nater - 12/21
listener(Key, Client) ->
  receive
    {l,_,ok} ->
      listener(Key,Client);
    {l,Qref,keyok} ->
      io:format("<< (~w) : The key ~w has been added~n", [Qref, Key]);
    {l,Qref,R} ->
      io:format("<< (~w) : The key ~w has the value ~w~n", [Qref, Key, R]),
      {myring,Client} ! {lookkeys, Key, R}
    after ?Timeout ->
      io:format("Time out !~n")
  end.


% generateKey(Nodes)
% Nodes : Array of all processes (nodes) inside the ring
%
% generate an unique and free key for the key and node IDs
% Quentin Nater - 12/21
generateKey(Nodes) ->
  if length(Nodes) > ?Mbit -> % check if the ring has a place
    io:format("<< (ERR) : The limit of nodes has been reached (~w)~n", [length(Nodes)]),
    NewId = 0;
  true ->
    timer:sleep(100),
    Id = key:generate(), % get a new key
    Check = [Id || {Ids,_} <- Nodes, Ids == Id], % check if the key is already used
    if length(Check) > 0 ->
      NewId = (lists:last(Check) + length(Nodes)) rem ?Mbit;
    true -> NewId = Id
    end,

    NewCheck = [NewId || {Ids,_} <- Nodes, Ids == NewId], % check if the new key is ok

    if length(NewCheck) == 0 ->
      ok;
    true -> 
      generateKey(Nodes) % retry
    end
  end,
  io:format(">> (I) : choosen key (~w)~n", [NewId]),
  NewId.

% generateKey()
%
% generate a key for the key and node IDs
% Quentin Nater - 12/21
generateKey() ->
  Id = key:generate(),
  Id.


% stop(Nodes)
% Nodes : Array of all processes (nodes) inside the ring
%
% Stop all processes
% Quentin Nater - 12/21
stop(Nodes) ->
    lists:foreach(fun({_,Node}) -> Node ! stop end, Nodes).


% =========================================================== %
% Interact with the system from outside                       %

% CLIENT -> SERVER -> CLIENT %

% receiver()
%
% Handle the message in the client side
% Quentin Nater - 12/21
receiver() ->
  io:format(">> wait.....~n", []),
  receive
    {get,Nodes} ->
      io:format(">> current nodes :~n"),
      [io:format(">> ID : ~w / Process : ~w ~n", [X,Y]) || {X,Y} <- Nodes],
      timer:sleep(200);
    {lookkeys, Key, R} ->
      io:format(">> The value of the key ~w is : ~w ~n", [Key, R]),
      timer:sleep(200);
    {get, keys, Key} ->
      io:format(">> The key ~w has been added ~n", [Key]),
      timer:sleep(200);
    {getnodes, UpdatedNodes} ->
      io:format(">> The new node has been added ~w~n", [UpdatedNodes]),
      timer:sleep(200)
    after ?Timeout ->
      io:format("Time out !~n")
  end.

% init()
%
% Set the process name in the client side
% Quentin Nater - 12/21 
init() ->
  register(myring, self()).


% getInfo()
% Host  :   Name of the server
% Client:   Name of the client
%
% Ask the log of the ring
% Quentin Nater - 12/21
getInfo(Host, Client) ->
  {myring,Host} ! {info, Client},
  receiver(),
  ok.


% lookFor(Host, Key, Client)
% Host  :   Name of the server
% Key   :   Key to search
% Client:   Name of the client
%
% Ask the value of a key and response to the client
% Quentin Nater - 12/21
lookFor(Host, Key, Client) ->
  {myring,Host} ! {h, lookfor, Key, Client},
  receiver(),
  ok.


% postKey(Host, Value, Client)
% Host  :   Name of the server
% Value :   Value of the new key
% Client:   Name of the client
%
% Create a new key with automatic ID and response to the client
% Quentin Nater - 12/21
postKey(Host, Value, Client) ->
  {myring,Host} ! {h, addkey, Value, Client},
  receiver(),
  ok.

% postKey(Host, Key, Value, Client)
% Host  :   Name of the server
% Key   :   Id of the new key
% Value :   Value of the new key
% Client:   Name of the client
%
% Create a new key and response to the client
% Quentin Nater - 12/21
postKey(Host, Key, Value, Client) ->
  {myring,Host} ! {h, addkey, Key, Value, Client},
  receiver(),
  ok.


% login(Host, Client)
% Host  :   Name of the server
% Client:   Name of the client
%
% Create a new node/process with automatic Id and response to the client
% Quentin Nater - 12/21
login(Host, Client) ->
  {myring,Host} ! {h, addnode, Client},
  receiver(),
  ok.

% login(Host, Key, Client)
% Host  :   Name of the server
% Key   :   Id of the new node
% Client:   Name of the client
%
% Create a new node/process and response to the client
% Quentin Nater - 12/21
login(Host, Key, Client) ->
  {myring,Host} ! {h, addnode, Key, Client},
  receiver(),
  ok.
