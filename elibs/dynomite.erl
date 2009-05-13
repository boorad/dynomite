-module(dynomite).

-export([start/0, running/1, running_nodes/0, pause_all_sync/0, start_all_sync/0, stop/0, restart/0]).

-include("../include/common.hrl").

start() ->
  crypto:start(),
  ensure_started([os_mon, thrift, mochiweb, dynomite]).

stop() ->
  application:stop(dynomite).

restart() ->
  stop(),
  start().

% running(Node) when Node == node() ->
%   true;

running(Node) ->
  Ref = erlang:monitor(process, {membership, Node}),
  R = receive
    {'DOWN', Ref, _, _, _} -> false
  after 1 ->
    true
  end,
  erlang:demonitor(Ref),
  R.

running_nodes() ->
  [Node || Node <- nodes([this,visible]), dynomite:running(Node)].

pause_all_sync() ->
  SyncServers = lists:flatten(lists:map(fun(Node) ->
      rpc:call(Node, sync_manager, loaded, [])
    end, running_nodes())),
  lists:foreach(fun(Server) ->
      sync_server:pause(Server)
    end, SyncServers).

start_all_sync() ->
  SyncServers = lists:flatten(lists:map(fun(Node) ->
      rpc:call(Node, sync_manager, loaded, [])
    end, running_nodes())),
  lists:foreach(fun(Server) ->
      sync_server:play(Server)
    end, SyncServers).

%%==============================================================

ensure_started([]) ->
  ok;
ensure_started([App|Apps]) ->
  case application:start(App) of
    ok ->
      ensure_started(Apps);
    {error, {already_started, App}} ->
      ?infoFmt("already started: ~p~n", [App]),
      ensure_started(Apps);
    Err ->
      ?infoFmt("error starting ~p: ~p~n", [App, Err])
  end.

collect_loop() ->
  process_flag(trap_exit, true),
  Filename = io_lib:format("/home/cliff/dumps/~w-dyn.dump", [lib_misc:now_int()]),
  sys_info(Filename),
  receive
    nothing -> ok
  after 5000 -> collect_loop()
  end.

sys_info(Filename) ->
  {ok, IO} = file:open(Filename, [write]),
  ok = io:format(IO, "count ~p~n", [erlang:system_info(process_count)]),
  ok = io:format(IO, "memory ~p~n", [erlang:memory()]),
  ok = file:write(IO, erlang:system_info(procs)),
  file:close(IO).
