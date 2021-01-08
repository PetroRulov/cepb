-module(cepb_cache_server).
-behaviour(gen_server).

-include("../include/cepb_header.hrl").
-include("../include/logger.hrl").

-export([init/1, start_link/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% LAUNCH SERVER
-spec start_link(atom(), list()) -> {ok, pid()}.
start_link(TableName, [{drop_interval, Time}]) ->
  DropInterval = Time * 1000,
  gen_server:start_link({local, TableName}, ?MODULE, [TableName, DropInterval], [timeout, 5000]).

init([TableName, DropInterval]) ->
  create(TableName),
  erlang:send_after(DropInterval, whereis(TableName), {delete_obsolete, {TableName, DropInterval}}),
  State = ets:tab2list(TableName),
  {ok, State}.

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

handle_call({lookup, {TableName, Key}}, _From, State) ->
  Val = lookup(TableName, Key),
  case Val of
    {ok, Value} ->
      {reply, {ok, Value}, State};
    _ ->
      {noreply, State}
  end;
handle_call(_, _From, State) ->
  {noreply, State}.

handle_cast({insert, {TableName, Key, Value, LifeTime}}, _State) ->
  NewState = insert(TableName, Key, Value, LifeTime),
  {noreply, NewState};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_info({delete, {TableName, Key}}, _State) ->
  NewState = ets:delete(TableName, Key),
  {noreply, NewState};
handle_info({delete_obsolete, {TableName, _DropInterval}}, _State) ->
  NewState = delete_obsolete(TableName),
  erlang:send_after(1, self(), {delete_obsolete, {TableName, 1}}),
  {noreply, NewState};
handle_info(_, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(Reason, _State) ->
  Reason.


%% - a u x i l i a r y   f u n c t i o n s -
-spec create(TableName::atom()) -> ok | error.
create(TableName) ->
  case lists:member(TableName, ets:all()) of
    false ->
      %% ets:new(TableName, [protected, named_table, set]),
      Options = [protected, named_table, set],
      cepb_ets_mng:new(TableName, Options),
      ok;
    true ->
      ?ERROR("Reason: ets-table ~p exists already!~n", [TableName]),
      error
  end.

-spec insert(TableName::atom(), Key::integer(), Value::term(), LifeTime::integer()) -> ok | error.
insert(TableName, Key, Value, LifeTime) when is_integer(LifeTime) ->
  try
    ets:insert(TableName, {Key, Value, erlang:localtime(), (element(2, erlang:timestamp()) + LifeTime)}),
    ok
  catch
    error:badarg ->
      ?ERROR("Reason: bad ets_table_name arg: ~p~n", [TableName]),
      error
  end;
insert(_, _, _, LifeTime) ->
  ?ERROR("Reason: bade life_time arg:~p~n", [LifeTime]),
  error.

-spec delete_obsolete(TableName::atom) -> ok | error.
delete_obsolete(TableName) ->
  try
    OldContent = ets:tab2list(TableName),
    CurrentTime = element(2, erlang:timestamp()),
    NewContent = lists:filter(fun({_, _, _, LifeTime}) ->
      LifeTime >= CurrentTime end, OldContent),
    ets:delete_all_objects(TableName),
    ets:insert(TableName, NewContent),
    ok
  catch
    error:badarg ->
      ?ERROR("Reason: absent ets_table_name:~p~n", [TableName]),
      error
  end.

-spec lookup(TableName::atom(), Key::integer()) -> {ok, term()} | {error, badarg}.
lookup(TableName, Key) ->
  try
    {_, Value, _, _} = hd(ets:lookup(TableName, Key)),
    {ok, Value}
  catch
    error:badarg ->
      ?ERROR("Reason: bad ets_table_name arg:~p or bad Key arg: ~p~n", [TableName, Key]),
      {error, badarg}
  end.