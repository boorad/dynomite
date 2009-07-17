-module(test_utils).
-author('cliff@powerset.com').

-export([priv_dir/0, priv_file/1]).


priv_dir() ->
  Dir = filename:join([t:config(priv_dir), "data", atom_to_list(?MODULE),
                       pid_to_list(self())]),
  filelib:ensure_dir(filename:join([Dir, atom_to_list(?MODULE)])),
  Dir.


priv_file(File) ->
  filename:join(priv_dir(), File).