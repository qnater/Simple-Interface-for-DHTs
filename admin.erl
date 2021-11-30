-module(admin).
-export([start/0, startRing/1, startRing/3, stop/1, listener/1, lookFor/2, addKey/3]).


start() ->
    startRing(4).


startRing(N) ->
    Key = key:generate(),
    StarterPid  = node:deploy(Key, nil, []),
    io:format("..startRing (StarterPid) : ~w ~w....................~n", [Key, StarterPid]),
    startRing(N-1, StarterPid, [{Key, StarterPid}]).


startRing(N,Spid,Nodes) when N > 0 ->
    Key = key:generate(),
    Pid = node:deploy(Key, Spid, []),
    io:format("..startRing (StarterPid) : ~w ~w....................~n", [Key, Pid]),
    timer:sleep(2000),
    startRing(N-1,Spid,[{Key,Pid}|Nodes]);
    
   
startRing(_,_,Nodes) -> 
  % KEY FOR TESTS.......................................
  KeyAlpha  = key:generate(),
  KeyBeta   = KeyAlpha + 1,
  KeyGamma  = key:generate(),
  KeyDelta = key:generate(),

  % FIRST AND LAST PROCESS..............................
  {_,Node} = lists:nth(1, Nodes),
  {_,Spid} = lists:last(Nodes),



  % DYNAMIC INSERT OF KEY...............................
  addKey(KeyAlpha, 31415, Node),
  addKey(KeyBeta, 9265, Node),
  addKey(KeyGamma, 3596, Node),



  % DYMAMIC INSTER OF NODE.............................
  UpdatedNodes_0 = addNode(KeyDelta, Spid, Nodes),
  UpdatedNodes = addNode(KeyDelta+1, Spid, UpdatedNodes_0),
  timer:sleep(2000),

  % LOOK FOR A KEY.....................................
  lookFor(KeyAlpha, Spid),



  % LOGS...............................................
  [Y!logs || {_,Y} <- UpdatedNodes],
  io:format(">> Nodes : ~w ~n", [UpdatedNodes]),

  Nodes.


listener(Key) ->
  receive
    {_,ok} ->
      listener(Key);
    {Qref,keyok} ->
      io:format("<< (~w) : The key ~w has been added~n", [Qref, Key]);
    {Qref,R} ->
      io:format("<< (~w) : The key ~w has the value ~w~n", [Qref, Key, R])
  end.


lookFor(Key, Spid) ->
  node:getEntry(Key, Spid, self()),
  listener(Key).

addKey(Key, Value, Node) ->
  node:addEntry(Key, Value, Node, self()),
  listener(Key).

addNode(Key, LastProcess, Nodes) ->
  Pid = node:deploy(Key, LastProcess, []),
  UpdatedNodes = [{Key,Pid}|Nodes],
  UpdatedNodes.


stop(Nodes) ->
    lists:foreach(fun({_,Node}) -> Node ! stop end, Nodes).