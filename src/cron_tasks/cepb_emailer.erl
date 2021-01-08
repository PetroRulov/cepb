-module(cepb_emailer).

-include("../include/cepb_header.hrl").
-include("../include/logger.hrl").

%% API
-export([init/1, send_rates/6]).

%% ===================================================================
%% API functions
%% ===================================================================

init(Config) ->
  Sender = proplists:get_value(sender, Config),
  Password = proplists:get_value(password, Config),
  Port = proplists:get_value(port, Config),
  Receivers = proplists:get_value(receivers, Config),
  Relay = proplists:get_value(relay, Config),
  InTime = proplists:get_value(cron_time, Config),
  Subject = ?TO_BIN(proplists:get_value(subject, Config)),
  Job = {{daily, InTime}, {cepb_emailer, send_rates, [Sender, Password, Port, Receivers, Relay, Subject]}},
  erlcron:cron(Job),
  ?INFO("===============================CURREX_SEND_EMAILS_JOB_TRIGGERED==============================~n", []),
  ok.

send_rates(Sender, Password, Port, Receivers, Relay, Subject) ->
	Answer = ?TO_BIN(currency_exchange:get_rates()),
  %Answer = compose_content(),
  Content = <<"Subject:", Subject/binary, "\r\n\r\n", Answer/binary,"">>,
  Options = [
		{relay, Relay}, {ssl, true}, {auth, always}, {port, Port}, {username, Sender}, {password, Password}
	],
  case gen_smtp_client:send({Sender, Receivers, Content}, Options) of
    {'ok', _Pid} ->
      ?INFO("===============================EMAILS_WERE_SENT_SUCESSFULLY===============================~n", []);
    {'error', Error} ->
      ?INFO("Sending emails error: ~p~n", [Error])
  end.

compose_content() ->
  Data = 
  [
    [
      {<<"User_Id">>, <<"1008703700116">>},
      {<<"Email">>, <<"ivanoff@gmail.com">>},
      {<<"Last name">>, <<"Ivanoff">>},
      {<<"First name">>, <<"Petr">>},
      {<<"Last Visit">>, <<"2019/05/22 T 23:59:59">>}
    ],
    [
      {<<"User_Id">>, <<"1008703700117">>},
      {<<"Email">>, <<"petroff@gmail.com">>},
      {<<"Last name">>, <<"Petroff">>},
      {<<"First name">>, <<"Ivan">>},
      {<<"Last Visit">>, <<"2019/05/17 T 13:02:57">>}
    ],
    [
      {<<"User_Id">>, <<"1008703700118">>},
      {<<"Email">>, <<"vasechkin@gmail.com">>},
      {<<"Last name">>, <<"Vasechkin">>},
      {<<"First name">>, <<"undefined">>},
      {<<"Last Visit">>, <<"2019/05/18 T 19:23:16">>}
    ]
  ],
  Msg = [jsx:encode(PL) || PL <- Data],
  << <<X/binary, "\n">> || X <- Msg>>.