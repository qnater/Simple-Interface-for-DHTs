-module(node).

-export([launch/1, find_successor/1, notify/3, request/2, stabilize/3, handle_node/1, node/3]).

-define(Mbit,64).

launch(N) ->
  
  % CHECK
  timer:start(),

  {ok, L} = inet:getif(),
  IP = element(1, hd(L)),
  io:format("~nNumber IP ~w : ~w ~n~n", [N, IP]),


  Keys = [erlang:phash2("key1") rem ?Mbit, erlang:phash2("key2") rem ?Mbit],
  io:format("Keys :~w ~n", [Keys]), 
  

  IPs = [IP, {172,18,0,67}, {192,168,0,10}, {10,0,0,15},{172,18,0,36}],


  Ring = [spawn(?MODULE, handle_node, [X]) || X <- IPs],
 
  
  io:format("Ring ~w ~n", [Ring]).


find_successor(id) ->
  io:format("todo").
  
node(Id,Predecessor,Successor) ->
 receive
  {key,Qref,Peer} ->
    %Peer : node asking, QRef : the key
    Peer ! {Qref, Id},
    node(Id, Predecessor, Successor);
  {notify,New} ->
    Pred = notify(New, Id, Predecessor),
    node(Id, Pred, Successor);
  {request, Peer} ->
    request(Peer, Predecessor),
    node(Id, Predecessor, Successor);
  {status, Pred} ->
    Succ = stabilize(Pred, Id, Successor),
    node(Id, Predecessor, Succ)
  end.

notify({NewKey,NewPid},Id,Predecessor) ->
 io:format("todo, notify a node").

request(Peer, Predecessor) ->
  io:format("todo, request for a key").

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,

  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Id, _} ->
      Successor;
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Xkey, Xpid} ->
      case key:between(Id, Xkey, Skey) of
      % If the key of the predecessor of our successor (Xkey)
      % is between us and our successor we should of course adopt this node as our
      % successor and run stabilization again.
      false ->
          Xpid ! {request, self()},
          io:format("~w : updates successor from ~w to ~w ~n",[Id, Skey, Xkey]),
          Pred;
      true -> 
          Spid ! {notify, {Id, self()}},
          io:format("~w : sending notify to ~w (pred : ~w )~n",[Id, Skey, Xkey]),
          Successor
      end
  end,
  io:format("todo, stabilize our nodes").

handle_node(Id) ->
  NodeId = erlang:phash2(Id) rem ?Mbit,
  io:format("~nStarting nodes : '~w'~nNode ID : ~w~n", [Id, NodeId]),
  
  node(Id,{},{}).

  



