-module(node).

-export([deploy/1, deploy/3, connect/2, notify/4, request/2, stabilize/3, node/4, handover/3, add/8, lookup/7,addEntry/4, getEntry/3]).

%Size of the ring
-define(Mbit,64).
%Time before a node will perfom the stabilize method
-define(TimeToStabilize, 100).
%Timeout
-define(Timeout, 10000).


% deploy(N)
% Id : Id of the node 
%
% Deploy a new node
% Quentin Nater - 12/21
deploy(Id) ->
  deploy(Id, nil, []).


% deploy(Id, Peer, Store)
% Id    : Id of the node 
% Peer  : Process to connect
% Store : Current keys of the node
%
% Deploy a new node with information
% Quentin Nater - 12/21
deploy(Id, Peer, Store) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer, Store) end).


% init(Id, Peer, Store)
% Id    : Id of the node 
% Peer  : Process to connect
% Store : Current keys of the node
%
% Initialize and connect with other nodes
% Quentin Nater - 12/21
init(Id, Peer, Store) ->
  Predecessor = nil,
  {ok,Successor} = connect(Id, Peer),
  handle_stabilize(),
  node(Id,Predecessor,Successor, Store).


% handle_stabilize()
% 
% Start each X second the stabilization
% Quentin Nater - 12/21
handle_stabilize() ->
  timer:send_interval(?TimeToStabilize, self(), stabilize).


% connect(Id, nil)
% Id    : Id of the node 
% nil   : No connection
%
% Start a new connection
% Quentin Nater - 12/21
connect(Id, nil) ->
  % When we enter, we are our own successor
  {ok, {Id, self()}};


% connect(_, Peer)
% _     : Id of the node 
% nil   : No connection
%
% Start a connection
% Quentin Nater - 12/21
connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey,Peer}}
  after ?Timeout ->
    io:format("Time out !~n")
  end.

% node(Id,Predecessor,Successor,Store)
% Id          : Id of the node 
% Predecessor : Predecessor of the node
% Successor   : Successor of the node
% Store : Current keys of the node
%
% Handle the communication between nodes
% Quentin Nater - 12/21
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

    % ... Informations/Tests/Actions ...........................
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
      io:format("RESULT> ~w -> Pred:~w Succ:~w STORE:~w~n",[Id,Predecessor,Successor,Store]),
      node(Id, Predecessor, Successor, Store)
  end.


% add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store)
% Key         : Key to handle
% Value       : Value of the key
% Qref        : Current reference
% Client      : Client who asks the adding
% Id          : Id of the node 
% Predecessor : Predecessor of the node
% Successor   : Successor of the node
% Store       : Current keys of the node
%
% add a key value pair, return the updated store
% Quentin Nater - 12/21
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
      true ->
          Client ! {l, Qref, keyok},
          lists:keystore(Key, 1, Store, {Key, Value}); 
      false ->
          Spid ! {add, Key, Value, Qref, Client},
          Store
  end.

% lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store)
% Key         : Key to handle
% Qref        : Current reference
% Client      : Client who asks the adding
% Id          : Id of the node 
% Predecessor : Predecessor of the node
% Successor   : Successor of the node
% Store       : Current keys of the node
%
% return a tuple {Key, Value} or the atom false
% Quentin Nater - 12/21
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = lists:keyfind(Key, 1, Store),
            io:format(">> ~w (L) : Lookup result for ~w : ~w~n",[Id,Key,Result]),
            {_,R} = Result,
            Client ! {l, Qref,R};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
        end.


% handover(Store, Nkey, Npid)
% Store       : Current keys of the node
% Nkey        : Id of the node
% Npid        : Process of the node
%
% Give the key to some other node
% Quentin Nater - 12/21
handover(Store, Nkey, Npid) ->
  {Leave, Keep} = lists:partition(fun({K,_}) -> K =< Nkey end, Store),
  Npid ! {handover, Leave},
  Keep.



% notify({Nkey, Npid}, Id, Predecessor, Store)
% Nkey        : Id of the node
% Npid        : Process of the node
% Id          : Id of the node 
% Predecessor : Predecessor of the node
% Store       : Current keys of the node
%
% A way for a node to make a friendly proposal that it might be our proper predecessor. 
% Quentin Nater - 12/21
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



% request(Peer, Predecessor)
% Peer        : Process to connect
% Predecessor : Predecessor of the node
%
% request information about the current status
% Quentin Nater - 12/21
request(Peer, Predecessor) ->
 case Predecessor of
  nil ->
    Peer ! {status, nil};
  {Pkey, Ppid} ->
    Peer ! {status, {Pkey, Ppid}}
  end.


% stabilize(stabilize, Predecessor)
% Predecessor : Predecessor of the node
% Id          : Id of the node 
% Successor   : Successor of the node
%
% communicate to update the status of the current predecessor and successor
% Quentin Nater - 12/21
stabilize(Predecessor, Id, Successor) ->
  {Skey, Spid} = Successor,

  case Predecessor of
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
          Predecessor;
      true -> 
          Spid ! {notify, {Id, self()}},
          io:format("~w (N) : sending notify to ~w (pred : ~w)~n",[Id, Skey, Xkey]),
          Successor
      end
  end.


% addEntry(Key, Value, NodePid, Client)
% Key         : Key to handle
% Value       : Value of the key
% NodePid     : Id of a node inside the ring
% Client      : Client who asks the adding
%
% Add a new key in the ring
% Quentin Nater - 12/21
addEntry(Key, Value, NodePid, Client) ->
  Qref = make_ref(),
  NodePid ! {add, Key, Value, Qref, Client},
  Qref.


% addEntry(Key, Value, NodePid, Client)
% Key         : Key to handle
% NodePid     : Id of a node inside the ring
% Client      : Client who asks the adding
%
% Get the value of a key inside the ring
% Quentin Nater - 12/21
getEntry(Key, NodePid, Client) ->
  Qref = make_ref(),
  NodePid ! {lookup, Key, Qref, Client},
  Qref.