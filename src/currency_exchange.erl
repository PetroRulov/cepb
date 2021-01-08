%%%-------------------------------------------------------------------
%%% @author Petro Rulov
%%% @copyright (C) 2020, "prulov777"
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2020 17:54
%%%-------------------------------------------------------------------
-module(currency_exchange).

-include("../include/cepb_header.hrl").
-include("../include/logger.hrl").

%% API
-export([init/1, get_rates/0, get_answer/0]).

init(InitParams) ->
  case lists:member(currency_exchange_rates, ets:all()) of
    true ->
      ok;
    false ->
      create_cache_server()
  end,
  ?INFO("CURRENCY_EXCHANGE started... ~p~n", [InitParams]),
  ok.

create_cache_server() ->
  {ok, TTL_Config} = application:get_env(cepb, cache_ttl),
  CacheTTL = ?TO_INT(proplists:get_value(currency_exchange, TTL_Config)),
  ?INFO("DATA CURRENCY_EXCHANGE RATES TTL: ~p sec ~n", [CacheTTL]),
  {ok, _} = cepb_cache_server:start_link(currency_exchange_rates, [{drop_interval, CacheTTL}]),
  ?INFO("~p ets-table created: ~p~n", [currency_exchange_rates, ets:tab2list(currency_exchange_rates)]),
  ok.

insert(Key, Value) ->
  {ok, TTL_Config} = application:get_env(cepb, cache_ttl),
  CacheTTL = ?TO_INT(proplists:get_value(currency_exchange, TTL_Config)),
  ?INFO("CurrentTime: ~p LifeTime: ~p~n", [element(2, erlang:timestamp()), (element(2, erlang:timestamp()) + CacheTTL)]),
  gen_server:cast(currency_exchange_rates, {insert, {currency_exchange_rates, Key, Value, CacheTTL}}).

lookup(Key) ->
  gen_server:call(currency_exchange_rates, {lookup, {currency_exchange_rates, Key}}).

get_rates() ->
  {Date, {ok, Answer}} = get_answer(),
  XML = create_xml(Date, jsx:decode(list_to_binary(Answer))),
  XML.

get_answer() ->
	case ets:first(currency_exchange_rates) of
		'$end_of_table' ->
           	case send_exchange_coursid5_request() of
           		{ok, Key} ->
           			{Key, lookup(Key)};
           		{error, Error} ->
           			?ERROR("~p~n",[Error])
           	end;
		Date ->
           {Date, lookup(Date)}
    end.

send_exchange_coursid5_request() ->
    GetBody = "json&exchange&coursid=5",
    {ok, URL_Config} = application:get_env(cepb, pb_urls),
    CE_URL = proplists:get_value(currex, URL_Config),
    FullURL = CE_URL ++ "?" ++ GetBody,
    Request = {FullURL, []},
    Response = get_response(Request),
    Response.

get_response(Request) ->
  case httpc:request(get, Request, [], [], default) of
    {ok, {{"HTTP/1.1",200,"OK"}, PL, Answer}} ->
      Date = ?TO_BIN(proplists:get_value("date", PL)), 
      insert(Date, Answer),
      ?INFO("Date: ~p~nAnswer: ~p~n", [Date, Answer]),
      {ok, Date};
    Error ->
      {error, Error}
  end.

create_xml(Date, Fields) ->
    Rows = create_rows(Fields, []),
    Data = [{exchangerates, [{current, [{date, Date}], []}] ++ Rows}],
    lists:flatten(xmerl:export_simple(Data, xmerl_xml)).

create_rows([], Acc) -> 
    Acc;
create_rows([Row | Rest], Acc) ->
    NewAcc = Acc ++ [{row, [{exchangerate, create_tags(Row), []}]}],
    create_rows(Rest, NewAcc).

create_tags(Row) ->
    [
      {ccy,maps:get(<<"ccy">>, Row)},
      {base_ccy,maps:get(<<"base_ccy">>, Row)},
      {buy,maps:get(<<"buy">>, Row)},
      {sale,maps:get(<<"sale">>, Row)}
    ].