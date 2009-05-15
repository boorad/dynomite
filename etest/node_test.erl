-include_lib("eunit/include/eunit.hrl").
-include("../etest/test.hrl").

-define(NODEA, {a, ["d", "1", "4"]}).
-define(NODEB, {b, ["e", "3", "1"]}).
-define(NODEC, {c, ["f", "1", "2"]}).
-define(NODES, [?NODEA, ?NODEB, ?NODEC]).

-define(NODED, {d, ["d", "1"]}).
-define(UNEVEN_NODES, [?NODEA, ?NODEB, ?NODEC, ?NODED]).


distinct_basic_test() ->
  Distinct = distinct_attributes(?NODES),
  ?assertEqual(Distinct,
               [["d","e","f"],["1","3"],["1","2","4"]]).

distinct_uneven_test() ->
  Distinct = distinct_attributes(?UNEVEN_NODES),
  ?assertEqual(Distinct,
               [["d","e","f"],["1","3"]]).

distinct_blank_test() ->
  Distinct = distinct_attributes([a,b,c]),
  ?assertEqual(Distinct, [[]]).
