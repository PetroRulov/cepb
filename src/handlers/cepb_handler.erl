-module(cepb_handler).

-export([init/2]).

init(Req0, State) ->
	Body = currency_exchange:get_rates(),
	Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/xml">>}, Body, Req0),
	{ok, Req, State}.
