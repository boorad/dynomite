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

-ifdef(TEST).
-include("../etest/replication_test.erl").
-endif.

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
  _Distinct = distinct_attributes(Nodes),
  [].

%%====================================================================
%% Internal functions
%%====================================================================

distinct_attributes(Nodes) ->
  Attribs = lists:map(fun node:attributes/1, Nodes),
  ?debugFmt("~nAttribs : ~p~n", [Attribs]),
  Distinct = distinct({[], Attribs}),
  ?debugFmt("~nDistinct: ~p~n", [Distinct]),
  Distinct.

distinct({Result, done}) ->
  lists:reverse(Result);
distinct({Acc, Attribs}) ->
  Tails = lists:map(fun([_H|T]) -> T end, Attribs),
  %% if any of the attribute lists is exhausted, we are done, throw out the rest
  NewTails = case lists:member([], Tails) of
               true -> done;
               false -> Tails
             end,
  Heads = lists:foldl(fun([H|_T], AccIn) -> [H | AccIn] end, [], Attribs),
  NewHeads = lists:usort(Heads),
%%   ?debugFmt("~nNewHeadsAcc: ~p~nNewTails   : ~p~n",
%%             [[NewHeads | Acc], NewTails]),
  distinct({[NewHeads | Acc], NewTails}).
