-module(key).
-export([generate/0,between/3]).

%Size of the ring
-define(Mbit,64).


% generate()
%
% Generate an unique Id for the keys and nodes
% Quentin Nater - 12/21
generate() ->
  erlang:phash2(rand:uniform(100000)) rem ?Mbit.


% between()
% _     : Any key
% From  : First indication
% From  : First indication
%
% Check the successor and predecessor
% Quentin Nater - 12/21
between(_, From, From) -> true;

% between()
% Key   : Key to check
% From  : First indication
% To    : Last indication
%
% Check the successor and predecessor (From is smaller then To ->) 
% Quentin Nater - 12/21
between(Key, From, To) when From < To -> ((From < Key) and (Key < To));

% between()
% Key   : Key to check
% From  : First indication
% To    : Last indication
%
% Check the successor and predecessor (From is higher then To <-) 
% Quentin Nater - 12/21
between(Key, From, To) when From > To -> ((From < Key) or (Key < To)).