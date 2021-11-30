-module(run).
-export([start/0, startRing/1, stop/1]).


start() ->
    startRing(4).

startRing(N) ->
    Spid = node:deploy(key:generate(), nil),
    io:format(">> startRing (Spid) :~w ~n", [Spid]),
    startRing(N-1, Spid).

startRing(N,Spid) when N > 0 ->
    node:deploy(key:generate(), Spid),
    timer:sleep(2000),
    startRing(N-1,Spid);

startRing(_,Pid) -> Pid.

stop(Nodes) ->
    lists:foreach(
        fun(Node) -> Node ! stop end,
        Nodes
    ).