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
  [].
%%====================================================================
%% Internal functions
%%====================================================================
