-include_lib("eunit/include/eunit.hrl").
-include("../etest/test.hrl").

-define(NODEA, {a, ["d", "1", "4"]}).
-define(NODEB, {b, ["e", "3", "1"]}).
-define(NODEC, {c, ["f", "1", "2"]}).
-define(NODES, [?NODEA, ?NODEB, ?NODEC]).


partners_test() ->
  configuration:start_link(#config{n=2,r=1,w=1,q=6,directory=priv_dir()}),
  Partners = partners(?NODEA, ?NODES, configuration:get_config()),
  ?assertEqual(Partners, []),
  configuration:stop().
