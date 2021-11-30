-module(node).

-export([deploy/1, deploy/3, connect/2, notify/4, request/2, stabilize/3, node/4, create_probe/3, remove_probe/2, forward_probe/6, handover/3, add/8, lookup/7,addEntry/4, getEntry/3]).

%Size of the ring
-define(Mbit,64).
%Time before a node will perfom the stabilize method
-define(TimeToStabilize, 100).
%Timeout
-define(Timeout, 10000).

deploy(N) ->
  deploy(N, nil, []).

deploy(Id, Peer, Store) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer, Store) end).

init(Id, Peer, Store) ->
  Predecessor = nil,
  {ok,Successor} = connect(Id, Peer),
  handle_stabilize(),
  node(Id,Predecessor,Successor, Store).


handle_stabilize() ->
  timer:send_interval(?TimeToStabilize, self(), stabilize).


connect(Id, nil) ->
  % When we enter, we are our own successor
  {ok, {Id, self()}};
connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey,Peer}}
  after ?Timeout ->
    io:format("Time out !~n")
  end.

  
node(Id,Predecessor,Successor,Store) ->
  receive
  % ... Actions ......................................
  {key,Qref,Peer} -> % Peer : node asking, QRef : the key / a peer needs to know our key
    Peer ! {Qref, Id}, 
    node(Id, Predecessor, Successor, Store);
  {notify, New} -> % A new node informs us of its existence
    {Pred, Keep} = notify(New, Id, Predecessor, Store),
    node(Id, Pred, Successor, Keep);
  {request, Peer} -> % A predecessor needs to know our predecessor
    request(Peer, Predecessor),
    node(Id, Predecessor, Successor, Store);
  {status, Pred} -> % Our successor informs us about its predecessor
    Succ = stabilize(Pred, Id, Successor),
    node(Id, Predecessor, Succ, Store);
  stabilize -> % stabilize the ring
    {_, Spid} = Successor,
    Spid ! {request, self()},
    node(Id, Predecessor, Successor, Store);

  % ... Informations/Tests ...........................
  probe ->
    create_probe(Id, Successor, Store),
    node(Id, Predecessor, Successor, Store);
  {probe, Id, Nodes, T} ->
    remove_probe(T, Nodes),
    node(Id, Predecessor, Successor, Store);
  {probe, Ref, Nodes, T} ->
    forward_probe(Ref, T, Nodes, Id, Successor, Store),
    node(Id, Predecessor, Successor, Store);
  {add, Key, Value, Qref, Client} ->
    Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
    node(Id, Predecessor, Successor, Added);
  {lookup, Key, Qref, Client} ->
    lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
    node(Id, Predecessor, Successor, Store);
  {handover, Elements} ->
    Merged = lists:keymerge(1, Elements, Store),
    node(Id, Predecessor, Successor, Merged);
  logs ->
    io:format("RESULT> ~w -> Pred:~w Succ:~w STORE:~w~n",[Id,Predecessor,Successor,Store])
  end.

% add a key value pair, return the updated store
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
      true ->
          Client ! {Qref, keyok},
          lists:keystore(Key, 1, Store, {Key, Value}); 
      false ->
          Spid ! {add, Key, Value, Qref, Client},
          Store
  end.

% return a tuple {Key, Value} or the atom false
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = lists:keyfind(Key, 1, Store),
            io:format(">> ~w (L) : Lookup resut : ~w~n",[Id,Result]),
            {_,R} = Result,
            Client ! {Qref,R};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
        end.



handover(Store, Nkey, Npid) ->
  {Leave, Keep} = lists:partition(fun({K,_}) -> K =< Nkey end, Store),
  Npid ! {handover, Leave},
  Keep.


% A way for a node to make a friendly proposal that it might be our proper predecessor. 
notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->  
            Keep = handover(Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            % We can not take their word for it, so we have to do our own investigation.
            case key:between(Nkey, Pkey, Id) of
                true -> 
                    Keep = handover(Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->  
                    {Predecessor, Store}
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
          io:format("~w (U) : updates successor from ~w to ~w ~n",[Id, Skey, Xkey]),
          Pred;
      true -> 
          Spid ! {notify, {Id, self()}},
          io:format("~w (N) : sending notify to ~w (pred : ~w)~n",[Id, Skey, Xkey]),
          Successor
      end
  end.

create_probe(Id, Successor, Store) ->
  {_, Spid} = Successor,
  Spid ! {probe, Id, [{Id, self(), Store}], erlang:timestamp()}.


remove_probe(T, Nodes) ->
  io:format("Probe : ~n",[]),
  lists:foreach(
    fun({Key, _, Store}) -> 
            io:format("    Node ~w :~n", [Key]),
            io:format("       Store:~n",[]), 
            lists:foreach(
              fun({K,V}) ->
                  io:format("        [~w] -> [~w]~n", [K, V]) 
              end,
              Store)
    end,
    Nodes),
  io:format("Took ~w ms.~n",[timer:now_diff(erlang:timestamp(), T)]).

forward_probe(Ref, T, Nodes, Id, Successor, Store) ->
  {_, Spid} = Successor,
  Spid ! {probe, Ref, [{Id, self(), Store}|Nodes], T}.


addEntry(Key, Value, NodePid, Client) ->
  Qref = make_ref(),
  NodePid ! {add, Key, Value, Qref, Client},
  Qref.


getEntry(Key, NodePid, Client) ->
  Qref = make_ref(),
  NodePid ! {lookup, Key, Qref, Client},
  Qref.