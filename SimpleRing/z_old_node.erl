-module(node).

-export([launch/1, deploy/1, deploy/2, connect/2, notify/3, request/2, stabilize/3, handle_node/1, node/3, create_probe/2, remove_probe/2, forward_probe/5]).

%Size of the ring
-define(Mbit,64).
%Time before a node will perfom the stabilize method
-define(TimeToStabilize, 100).
%Timeout
-define(Timeout, 10000).

launch(N) ->
  
  % CHECK
  timer:start(),

  {ok, L} = inet:getif(),
  IP = element(1, hd(L)),
  io:format("~nNumber IP ~w : ~w ~n~n", [N, IP]),

  Keys = [erlang:phash2("key1") rem ?Mbit, erlang:phash2("key2") rem ?Mbit],
  io:format("Keys :~w ~n", [Keys]), 
  
  IPs = [IP, {172,18,0,67}, {192,168,0,10}, {10,0,0,15}],

  Ring = [spawn(?MODULE, deploy, [erlang:phash2(X) rem ?Mbit]) || X <- IPs],

  timer:sleep(2000),

  io:format("Ring ~w / 1st : ~w~n", [Ring,lists:nth(1, Ring)]),

  lists:nth(1, Ring) ! stabilize.


deploy(N) ->
  deploy(N, nil).

deploy(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).
  

init(Id, Peer) ->
  Predecessor = nil,
  {ok,Successor} = connect(Id, Peer),
  handle_stabilize(),
  node(Id,Predecessor,Successor).


handle_stabilize() ->
  timer:send_interval(?TimeToStabilize, self(), stabilize).


connect(Id, nil) ->
  % When we enter, we are our own successor
  io:format("New one connect  ~w ~n", [Id]),
  {ok, {Id, self()}};
connect(Id, Peer) ->
  io:format("New one connect  ~w ~n", [Id]),
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey,Peer}}
  after ?Timeout ->
    io:format("Time out !~n")
  end.

  
node(Id,Predecessor,Successor) ->
  receive
  {key,Qref,Peer} ->
    %Peer : node asking, QRef : the key / a peer needs to know our key
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
    node(Id, Predecessor, Succ);
  stabilize -> 
    {_, Spid} = Successor,
    Spid ! {request, self()},
    node(Id, Predecessor, Successor);
  probe ->
    create_probe(Id, Successor),
    node(Id, Predecessor, Successor);
  {probe, Id, Nodes, T} ->
    remove_probe(T, Nodes),
    node(Id, Predecessor, Successor);
  {probe, Ref, Nodes, T} ->
    forward_probe(Ref, T, Nodes, Id, Successor),
    node(Id, Predecessor, Successor)
  end.

notify({NewKey,NewPid},Id,Predecessor) ->
 case Predecessor of
  nil ->
    {NewKey, NewPid};
  {Pkey, _} ->
    case key:between(NewKey, Pkey, Id) of
      true ->
        {NewKey, NewPid};
      false ->
        Predecessor
    end
  end.


request(Peer, Predecessor) ->
 case Predecessor of
  nil ->
    Peer ! {status, nil};
  {Pkey, Ppid} ->
    Peer ! {status, {Pkey, Ppid}}
  end.

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
  end.

create_probe(Id, Successor) ->
  {Skey, Spid} = Successor,
  Spid ! {probe, Id, [{Skey, Spid}], erlang:timestamp()}.

remove_probe(T, Nodes) ->
  io:format("Probe : ~n",[]),
  lists:foreach(
    fun({Key, _}) -> io:format("Node ~w ~n", [Key]) end, Nodes),
  io:format("Took ~w ms.~n",[timer:now_diff(erlang:timestamp(), T)]).

forward_probe(Ref, T, Nodes, _Id, Successor) ->
  {Skey, Spid} = Successor,
  Spid ! {probe, Ref, [{Skey, Spid}|Nodes], T}.


handle_node(Id) ->
  NodeId = erlang:phash2(Id) rem ?Mbit,
  io:format("~nStarting nodes : '~w'~nNode ID : ~w~n", [Id, NodeId]),  
  node(Id,{},{}).