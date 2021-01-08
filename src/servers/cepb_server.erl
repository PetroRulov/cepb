-module(cepb_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("../include/cepb_header.hrl").
-include("../include/logger.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([init/1, start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Args) ->
    init_currexpb_modules(),
    ?INFO("==========================CURRENCY_EXCHANGE_PB APPLICATION STARTED======================~n", []),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

init_currexpb_modules() ->
    {ok,Modules} = application:get_env(cepb, cepb_modules),
    lists:foreach(
        fun({Module, InitParams}) ->
            try
                Module:init(InitParams)
            catch
                _:T ->
                    ?ERROR("Module ~p not initialized! ~nReason: ~p ~p~n",
                        [?TO_STR(Module), T, erlang:get_stacktrace()])
            end
        end, Modules),
    ?INFO("=======================CURRENCY_EXCHANGE_SYSTEMS MODULES INITIALIZED====================~n", []).
