-module(cepb_hw_printer).

-include("../include/logger.hrl").

%% API
-export([init/1, print/1, print/0]).

%% ===================================================================
%% API functions
%% ===================================================================

init(Config) ->
  ?INFO("Cron_Task Config: ~p~n", [Config]),
  %% Text = proplists:get_value(text, Config),
  InTime = proplists:get_value(cron_time, Config),
  %% Job = {{daily, InTime}, {currexpb_hw_printer, print, [Text]}},
  Job = {{daily, InTime}, {cepb_hw_printer, print, []}},
  erlcron:cron(Job),
  ?INFO("=============================CURREX_PRINT_DATE_JOB_TRIGGERED============================~n", []),
  ok.

print(Text) ->
	ok = io:format("Cron Task Execution: ~p~n", [Text]).

print() ->
	Text = add_datetime(),
	ok = io:format("Cron Task Execution: ~p~n", [Text]).

-spec add_datetime() -> string().
add_datetime() ->
	%% StrTime = date_to_str(calendar:universal_time()),
	StrTime = date_to_str(calendar:local_time()),
	StrTime.
	%% , eps_utils:to_bin(StrTime).

date_to_str(Date) ->
	{{Year,Month,Day},{Hour,Minutes,Seconds}} = Date,
    integer_to_list(Year) ++"-"++[$0 + Month div 10, $0 + Month rem 10, $-, $0 + Day div 10, $0 + Day rem 10]++" "++
        [$0 + Hour div 10, $0 + Hour rem 10, $:, $0 + Minutes div 10, $0 + Minutes rem 10, $:, $0 + Seconds div 10, $0 + Seconds rem 10]
.
