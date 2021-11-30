-module(ring).
-export([start/0, startRing/1, startRing/3, stop/1, listener/1, lookFor/2, addKey/3, handling/1, addNode/3, addNode/1, lookFor/1, addKey/2, info/0]).


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

  % DYNAMIC INSERT OF KEY...............................
  addKey(KeyAlpha, 31415),
  addKey(KeyBeta, 9265),
  addKey(KeyGamma, 3596),

  % DYMAMIC INSTER OF NODE.............................
  addNode(KeyDelta),
  
  % LOOK FOR A KEY.....................................
  lookFor(KeyAlpha),

  % LOGS...............................................
  info(),

  handling(Nodes),

  Nodes.

handling(Nodes) ->
  {_,Process} = lists:last(Nodes),
  receive
    {h,lookfor,Key} ->
      node:getEntry(Key, Process, self()),
      listener(Key),
      timer:sleep(1000),
      handling(Nodes);
    {h,addkey,Key,Value} ->
      node:addEntry(Key, Value, Process, self()),
      listener(Key),
      timer:sleep(200),
      handling(Nodes);
    {h,addnode,Key} ->
      Pid = node:deploy(Key, Process, []),
      UpdatedNodes = [{Key,Pid}|Nodes],
      timer:sleep(1000),
      handling(UpdatedNodes);
    info ->
      [Y!logs || {_,Y} <- Nodes],
      io:format(">> Nodes : ~w ~n", [Nodes]),
      timer:sleep(200),
      handling(Nodes)
  end.


listener(Key) ->
  receive
    {_,ok} ->
      listener(Key);
    {Qref,keyok} ->
      io:format("<< (~w) : The key ~w has been added~n", [Qref, Key]);
    {Qref,R} ->
      io:format("<< (~w) : The key ~w has the value ~w~n", [Qref, Key, R])
  end.

info() ->
  self() ! info.

lookFor(Key) ->
  self() ! {h,lookfor,Key}.

addKey(Key, Value) ->
  self() ! {h,addkey,Key,Value}.

addNode(Key) ->
  self() ! {h,addnode,Key}.


lookFor(Key, Process) ->
  node:getEntry(Key, Process, self()),
  listener(Key),
  m,n.

addKey(Key, Value, Process) ->
  node:addEntry(Key, Value, Process, self()),
  listener(Key).

addNode(Key, Process, Nodes) ->
  Pid = node:deploy(Key, Process, []),
  UpdatedNodes = [{Key,Pid}|Nodes],
  UpdatedNodes.


stop(Nodes) ->
    lists:foreach(fun({_,Node}) -> Node ! stop end, Nodes).