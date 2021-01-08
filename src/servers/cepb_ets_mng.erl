-module(cepb_ets_mng).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).

-include("../include/logger.hrl").

%% API
-export([init/1, start_link/0, new/2]).
%% CALLBACK
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


init(_Args) ->
  State = dict:new(),
  ?INFO("===============================CURREX_PB_ETS_MANAGER STARTED============================~n", []),
  {ok, State}.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new(Name, Options) when is_atom(Name), is_list(Options) ->
  Key = {Name, Options},
  Server = whereis(?MODULE),
  case gen_server:call(Server, {get, Key}) of
    {ok, Tag} ->
      receive
        {'ets_transfer', Table, Server, Tag} ->
          Table
      after
        ?TIMEOUT ->
          exit(timeout)
      end;
    {error, not_found} ->
      HeirOpt = case lists:keymember(heir, 1, Options) of
            true ->
              Options;
            false ->
              [{heir, Server, Key} | Options]
          end,
      ets:new(Name,  HeirOpt)
  end.

%% CALLBACK FUNCTIONS %%
handle_call({get, Key}, {Process, Tag}, Tables) ->
  case dict:find(Key, Tables) of
    {ok, Table} ->
      true = ets:give_away(Table, Process, Tag),
      {reply, {ok, Tag}, dict:erase(Key, Tables)};
    error ->
      {reply, {error, not_found}, Tables}
  end;
handle_call(_Request, _From, State) ->
  {reply, {error, badarg}, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_info({'ets_transfer', Table, _, Key}, Tables) ->
  {noreply, dict:store(Key, Table, Tables)};
handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(Reason, _State) ->
  Reason.
