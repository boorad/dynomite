%%%-------------------------------------------------------------------
%%% File:      replication.erl
%%% @author    Brad Anderson <brad@cloudant.com> [http://www.cloudant.com]
%%% @copyright 2009 Brad Anderson
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-06-14 by Brad Anderson
%%%-------------------------------------------------------------------
-module(replication).
-author('brad@cloudant.com').

%% API
-export([partners/3]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/config.hrl").
-include("../include/common.hrl").


%%====================================================================
%% API
%%====================================================================


%%--------------------------------------------------------------------
%% @spec partners(Node::atom(), Nodes::list(), Config::config()) ->
%%          list()
%% @doc  returns the list of all replication partners for the specified node
%% @end
%%--------------------------------------------------------------------
partners(Node, Nodes, Config) ->
  N = Config#config.n,
  Meta = Config#config.meta,
  pick_partners(Meta, Node, Nodes, [], N - 1).


%%====================================================================
%% Internal functions
%%====================================================================

%% @spec pick_partners(proplist(), Node::dynomite_node(), [Node], [Node],
%%                     integer()) -> list()
%% @doc iterate through N-1 partner picks, returning the resulting list sorted
pick_partners(_Meta, Node, _Nodes, Acc, 0) ->
  lists:sort(lists:delete(Node, Acc));
pick_partners(Meta, Node, Nodes, Acc, Count) ->
  Partner = pick_partner(Meta, Node, Nodes, Acc, 1),
  pick_partners(Meta, Node, lists:delete(Partner, Nodes), [Partner|Acc],
                Count-1).


%% @spec pick_partner(proplist(), Node::dynomite_node(), [Node], [Node],
%%                    integer()) -> Node::dynomite_node()
%% @doc pick a specific replication partner at the given level
pick_partner([], Node, Nodes, _Acc, 1) ->
  %% handle the no metadata situation
  %% Note: This clause must be before the Level > length(Meta) guarded clause
  target_key(node:name(Node), lists:map(fun node:name/1, Nodes), roundrobin);

pick_partner(Meta, _Node, _Nodes, Acc, Level) when Level > length(Meta) ->
  Acc;

pick_partner(Meta, Node, Nodes, Acc, Level) ->
  MetaDict = meta_dict(Nodes, Level, dict:new()),
  NodeKey = lists:sublist(node:attributes(Node), Level),
  Keys = dict:fetch_keys(MetaDict),
  {_MetaName, Strategy} = lists:nth(Level, Meta),
  TargetKey = target_key(NodeKey, Keys, Strategy),
  Candidates = dict:fetch(TargetKey, MetaDict),
  case length(Candidates) of
    0 ->
      %% didn't find a candidate
      partner_not_found; %% um, handle this better
    1 ->
      %% found only one candidate, return it
      [Partner] = Candidates,
      Partner;
    _ ->
      pick_partner(Meta, Node, Nodes, Acc, Level + 1)
  end.


%% @doc construct a dict that holds the key of metadata values so far (up to
%%      the current level, and dynomite_node() list as the value.  This is used
%%      to select a partner in pick_partner/5
%% @end
meta_dict([], _Level, Dict) ->
  Dict;

meta_dict([Node|Rest], Level, Dict) ->
  Key = lists:sublist(node:attributes(Node), Level),
  DictNew = dict:append(Key, Node, Dict),
  meta_dict(Rest, Level, DictNew).


%% @spec target_key(term(), list(), Strategy::atom()) -> term()
%% @doc given the key and keys, sort the list of keys based on stragety (i.e.
%%      for roundrobin, sort them, put the NodeKey on the end of the list, and
%%      then return the head of the list as the target.
%% @end
%% TODO: moar strategies other than roundrobin?
target_key(NodeKey, Keys, roundrobin) ->
  SortedKeys = lists:sort(Keys),
  [TargetKey|_Rest] = target_list(NodeKey, SortedKeys),
  TargetKey.


%% @spec target_list(term(), list()) -> list()
%% @doc split the list of keys into 'lessthan NodeKey', NodeKey, and 'greaterthan
%%      Nodekey' and then put the lessthan section on the end of the list
%% @end
target_list(NodeKey, Keys) ->
  {A, [NodeKey|B]} = lists:splitwith(fun(K) -> K /= NodeKey end, Keys),
  lists:append([B, A, [NodeKey]]).
