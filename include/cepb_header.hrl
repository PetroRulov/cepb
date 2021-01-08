-define(TO_BIN, cepb_utils:to_bin).
-define(TO_STR, cepb_utils:to_str).
-define(TO_FLOAT, cepb_utils:to_float).
-define(TO_INT, cepb_utils:to_int).


-define(XML_RATE(Date, USD, EUR, RUR, BTC),
	<<"<exchangerates>", Date/binary, USD/binary, EUR/binary, RUR/binary, BTC/binary, "</exchangerates>">>
).

-define(DATE(Date),
	<<"<date>", Date/binary, "</date>">>
	).

-define(CUR_RATE(BaseCCY, Buy, CCY, Sale),
	<<"<row>
		<exchangerate ccy=\"", CCY/binary, "\" base_ccy=\"", 
		BaseCCY/binary, "\" buy=\"", Buy/binary, "\" sale=\"", Sale/binary, "\"/>
	</row>">>
).
