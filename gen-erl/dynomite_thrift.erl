%%
%% Autogenerated by Thrift
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(dynomite_thrift).
-behaviour(thrift_service).


-include("dynomite_thrift.hrl").

-export([struct_info/1, function_info/2]).

struct_info('i am a dummy struct') -> undefined.
%%% interface
% get(This, Key)
function_info('get', params_type) ->
  {struct, [{1, string}]}
;
function_info('get', reply_type) ->
  {struct, [{1, string},
  {2, {list, string}}]}
;
function_info('get', exceptions) ->
  {struct, [{1, {struct, {'dynomite_types', 'failureException'}}}]}
;
% put(This, Key, Context, Data)
function_info('put', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string}]}
;
function_info('put', reply_type) ->
  i32;
function_info('put', exceptions) ->
  {struct, [{1, {struct, {'dynomite_types', 'failureException'}}}]}
;
% has(This, Key)
function_info('has', params_type) ->
  {struct, [{1, string}]}
;
function_info('has', reply_type) ->
  i32;
function_info('has', exceptions) ->
  {struct, [{1, {struct, {'dynomite_types', 'failureException'}}}]}
;
% remove(This, Key)
function_info('remove', params_type) ->
  {struct, [{1, string}]}
;
function_info('remove', reply_type) ->
  i32;
function_info('remove', exceptions) ->
  {struct, [{1, {struct, {'dynomite_types', 'failureException'}}}]}
;
function_info(xxx, dummy) -> dummy.

