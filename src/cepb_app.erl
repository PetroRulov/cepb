-module(cepb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_',
            [
                {<<"/cepb">>, cepb_handler, []}
            ]
            }]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, 8088}],
        #{env => #{dispatch => Dispatch}}),
    cepb_sup:start_link().

stop(_State) ->
    ok.
