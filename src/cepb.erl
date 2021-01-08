-module(cepb).

%% API
-export([
  start/0,
  stop/0
]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
  {ok, _} = application:ensure_all_started(cepb, permanent).

stop() ->
  application:stop(cepb).

