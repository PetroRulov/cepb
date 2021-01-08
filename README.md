cepb
=====

An OTP application with the samples of:
1) erlcron usage (server for a postponed task's execution);
2) gen_smtp usage (email sending);
3) manager for storing data in ets-table with the ttl defined in a config (cache-server);
4) ets-table manager, preventing from the losing of ets-table data after accidentally crashing of a process (ets-table owner), with usage of a heir-process mechanism, dict and ets:give_away/3 func.  

Build
-----

    $ rebar3 compile
