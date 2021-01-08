cepb
=====

An OTP application with the samples of:
1) erlcron usage (server for a postponed task's execution);
2) gen_smtp usage (email ending);
3) manager for storing data in ets-table with the ttl defined in a config (cache-server);
4) manager for ets-tables (with usage of heir-process mechanism, dict and ets:give_away/3 func).  

Build
-----

    $ rebar3 compile
