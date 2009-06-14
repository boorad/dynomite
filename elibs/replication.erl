%%%-------------------------------------------------------------------
%%% File:      replication.erl
%%% @author    Cliff Moon <> []
%%% @copyright 2009 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-05-06 by Cliff Moon
%%%-------------------------------------------------------------------
-module(replication).
-author('cliff@powerset.com').

%% API
-export([partners/3]).

-include("../include/config.hrl").
-include("../include/common.hrl").
-include_lib("eunit/include/eunit.hrl").


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
  ?debugFmt("~nNode  : ~p~nNodes : ~p~nConfig: ~p~n", [Node, Nodes, Config]),
  N = Config#config.n,
  pick_partners(Node, Nodes, [], N - 1).


%%====================================================================
%% Internal functions
%%====================================================================

pick_partners(_Node, _Nodes, Acc, 0) ->
  Acc;
pick_partners(Node, Nodes, Acc, N) ->
  MetaLevels = 3, %% from config at some point
  Partner = pick_partner(Node, Nodes, Acc, 1, MetaLevels),
  pick_partners(Node, Nodes, [Partner|Acc], N-1).


pick_partner(_Node, _Nodes, Acc, Level, Levels) when Level > Levels ->
  Acc;

pick_partner(Node, Nodes, Acc, Level, Levels) ->
  MetaDict = meta_dict(Nodes, Level, dict:new()),
  NodeKey = lists:nth(Level, node:attributes(Node)),
  Keys = dict:fetch_keys(MetaDict),
  TargetKey = target_key(NodeKey, Keys, roundrobin),
  ?debugFmt("~n"
            "NodeKey  : ~p~n"
            "Keys     : ~p~n"
            "TargetKey: ~p~n",
            [NodeKey, Keys, TargetKey]),

  Candidates = dict:fetch(TargetKey, MetaDict),
  ?debugFmt("~nCandidates: ~p~n", [Candidates]),
  case length(Candidates) of
    0 ->
      %% didn't find a candidate
      partner_not_found; %% um, handle this better
    1 ->
      %% found only one candidate, return it
      [Partner] = Candidates,
      Partner;
    _ ->
      pick_partner(Node, Nodes, Acc, Level + 1, Levels)
  end.


meta_dict([], _Level, Dict) ->
  Dict;

meta_dict([Node|Rest], Level, Dict) ->
  Key = lists:nth(Level, node:attributes(Node)),
  DictNew = dict:append(Key, Node, Dict),
  meta_dict(Rest, Level, DictNew).


%% TODO: moar strategies other than roundrobin?
target_key(NodeKey, Keys, roundrobin) ->
  SortedKeys = lists:sort(Keys),
  TargetList = target_list(NodeKey, SortedKeys),
  ?debugFmt("~nTargetList: ~p~n", [TargetList]),
  [TargetKey|_Rest] = TargetList,
  TargetKey.


target_list(NodeKey, Keys) ->
  {A, [NodeKey|B]} = lists:splitwith(fun(K) -> K /= NodeKey end, Keys),
  lists:append([B, A, [NodeKey]]).


%%====================================================================
%% Test functions
%%====================================================================

-include("../etest/test.hrl").

-define(NODEA, {a, ["d", "1", "4"]}).
-define(NODEB, {b, ["e", "3", "1"]}).
-define(NODEC, {c, ["f", "1", "2"]}).
-define(NODES, [?NODEA, ?NODEB, ?NODEC]).

partners_test() ->
  configuration:start_link(#config{n=2,r=1,w=1,q=6,
                                   directory=priv_dir()}),
  Partners = partners(?NODEA, ?NODES, configuration:get_config()),
  ?assertEqual([?NODEB], Partners),
  configuration:stop().
