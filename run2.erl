-module(run2).
-export([start/0, startRing/1, startRing/3, stop/1, listener/1]).


start() ->
    startRing(4).


startRing(N) ->
    Key = key:generate(),
    StarterPid  = node2:deploy(Key, nil, []),
    io:format("..startRing (StarterPid) : ~w ~w....................~n", [Key, StarterPid]),
    startRing(N-1, StarterPid, [{Key, StarterPid}]).


startRing(N,Spid,Nodes) when N > 0 ->
    Key = key:generate(),
    Pid = node2:deploy(Key, Spid, []),
    io:format("..startRing (StarterPid) : ~w ~w....................~n", [Key, Pid]),
    timer:sleep(2000),
    startRing(N-1,Spid,[{Key,Pid}|Nodes]);
    
   
startRing(_,_,Nodes) -> 
  
  
  KeyAlpha  = key:generate(),
  KeyBeta   = KeyAlpha + 1,
  KeyGamma  = key:generate(),
  {_,Node} = lists:nth(1, Nodes),

  {_,Spid} = lists:last(Nodes),
  KeyDelta = key:generate(),

  

  % DYNAMIC INSERT OF KEY
  node2:addEntry(KeyAlpha, 31415, Node, self()),
  node2:addEntry(KeyBeta, 9265, Node, self()),
  node2:addEntry(KeyGamma, 3596, Node, self()),


  % DYMAMIC INSTER OF NODE
  Pid3  = node2:deploy(KeyDelta, Spid, []),
  UpdatedNodes = [{KeyDelta,Pid3}|Nodes],

  timer:sleep(2000),


  % LOOK FOR A KEY
  node2:getEntry(KeyAlpha, Spid, self()),
  listener(KeyAlpha),

  

  % LOGS
  [Y!logs || {_,Y} <- UpdatedNodes],
  io:format(">> Nodes : ~w ~n", [UpdatedNodes]),

  Nodes.

listener(Key) ->
  receive
    {_,ok} ->
      listener(Key);
    {Qref,R} ->
      io:format("<< (~w) : The key ~w has the value ~w~n", [Qref, Key, R])
  end.

  
stop(Nodes) ->
    lists:foreach(fun({_,Node}) -> Node ! stop end, Nodes).