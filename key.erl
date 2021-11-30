-module(key).
-export([generate/0,between/3]).

%Size of the ring
-define(Mbit,64).

generate() ->
  erlang:phash2(rand:uniform(100000)) rem ?Mbit.


between(_, From, From) -> true;

between(Key, From, To) when From < To -> ((From < Key) and (Key < To));

between(Key, From, To) when From > To -> ((From < Key) or (Key < To)).