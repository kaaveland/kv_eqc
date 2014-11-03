%% This file contains an implementation of a simple key-value store,
%% and a state-machine specification of it.

-module(kv).

-include_lib("eqc/include/eqc.hrl").
-compile({parse_transform,eqc_cover}).
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

%% The key-value store is managed by a server, and implemented as a binary tree.

start() ->
  catch unregister(kv),
  register(kv,spawn_link(fun() -> server(leaf) end)).

server(T) ->
  receive
    {insert,K,V} ->
      server(insert(K,V,T));
    {lookup,K,Pid} ->
      Pid ! lookup(K,T),
      server(T)
  after 5000 ->
      %% Our server dies after 5 seconds of inactivity... just so we
      %% don't fill the memory with idle servers.
      ok
  end.

insert(Key, Value, leaf) ->
  {node, leaf, Key, Value, leaf};
insert(Key, Value, {node, Left, NodeKey, _NodeValue, Right}) ->
  if Key < NodeKey ->
      insert(Key, Value, Left);
     Key == NodeKey ->
      {node, Left, Key, Value, Right};
     Key > NodeKey ->
      insert(Key, Value, Right)
  end.

lookup(_, leaf) ->
  false;
lookup(Key, {node, Left, NodeKey, NodeValue, Right}) ->
  if Key < NodeKey ->
      lookup(Key, Left);
     Key == NodeKey ->
      {Key, NodeValue};
     Key > NodeKey ->
      lookup(Key, Left)
  end.

%% State machine

initial_state() ->
  [].

%% insert

insert(K,V) ->
  kv ! {insert,K,V}.

insert_args(_) ->
  [key(),val()].

insert_next(S,_,[K,V]) ->
  lists:keystore(K,1,S,{K,V}).

%% lookup

lookup(K) ->
  kv ! {lookup,K,self()},
  receive Msg ->
       Msg
  end.

lookup_args(_) ->
  [key()].

lookup_post(S,[K],Res) ->
  eq(Res,lists:keyfind(K,1,S)).

%% Generators

key() ->
  nat().

val() ->
  nat().

%% Property

prop_kv() ->
  ?FORALL(Cmds, commands(?MODULE),
          begin
            start(),
            {H, S, Res} = run_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            aggregate(command_names(Cmds),
                                      ?IMPLIES(Res/=precondition,
                                               Res == ok)))
          end).
