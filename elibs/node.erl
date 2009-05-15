%%%-------------------------------------------------------------------
%%% File:      node.erl
%%% @author    Cliff Moon <> []
%%% @copyright 2009 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-05-11 by Cliff Moon
%%%-------------------------------------------------------------------
-module(node).
-author('cliff@powerset.com').

%% API
-export([name/1, attributes/1, distinct_attributes/1]).

-include("../include/common.hrl").

-ifdef(TEST).
-include("../etest/node_test.erl").
-endif.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec
%% @doc
%% @end
%%--------------------------------------------------------------------
name(Name) when is_atom(Name) ->
  Name;
name(Node) when is_tuple(Node) ->
  element(1, Node);
name(Node) ->
  Node.

attributes(Name) when is_atom(Name) ->
  [];
attributes(Node) when is_tuple(Node) ->
  element(2, Node);
attributes(_) ->
  [].

%% @spec distinct_attributes([Nodes::node()] -> [Metadata::term()]
%% @type node() - Dynomite node atom() or tuple()
%% @doc Take a list of node() types and extract its metadata into separate
%%      lists of sorted, distinct values.  Used for replication partner algos
%% @end
distinct_attributes(Nodes) ->
  Attribs = lists:map(fun node:attributes/1, Nodes),
  %% ?debugFmt("~nAttribs: ~p~n", [Attribs]),
  Distinct = distinct({[], Attribs}),
  %% ?debugFmt("~nDistinct: ~p~n", [Distinct]),
  Distinct.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc iterate through node attributes, collecting and usort'ing values
%%      and returning the distinct list of lists
%% @end
distinct({Result, done}) ->
  lists:reverse(Result);
distinct({Acc, Attribs}) ->
  Heads = lists:foldl(fun heads_fun/2, [], Attribs),
  NewHeads = lists:usort(Heads),
  Tails = lists:map(fun tail_fun/1, Attribs),
  %% if any of the attribute lists is exhausted or blank, we are done,
  %%  so throw out the rest.
  NewTails = case lists:member([], Tails) or lists:member(blank, Tails) of
               true -> done;
               false -> Tails
             end,
  distinct({[NewHeads | Acc], NewTails}).


%% @doc heads function for use in distinct/1 foldl and to handle blank list
heads_fun([], AccIn) ->
  AccIn;
heads_fun([H|_T], AccIn) ->
  [H | AccIn].


%% @doc tail function for use in distinct/1 map and to handle blank list
tail_fun([]) ->
  blank;
tail_fun([_H|T]) ->
  T.
