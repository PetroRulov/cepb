-module(cepb_utils).

%% API
-export([to_str/1, to_bin/1, to_float/1, to_int/1]).
-export([get_param_from_config/1, get_param_from_config/2]).

%% @doc universal converter to string(list)
-spec to_str(binary()|list()|integer()|atom()|float()) -> list().
to_str(X) when is_list(X) -> X;
to_str(X) when is_binary(X) -> binary_to_list(X);
to_str(X) when is_integer(X) -> integer_to_list(X);
to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_float(X) -> lists:flatten(io_lib:format("~.2f",[X])).

%% @doc universal converter to binary
-spec to_bin(binary()|list()|integer()|atom()|float()) -> binary().
to_bin(X) when is_binary(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X);
to_bin(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_bin(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_bin(X) when is_float(X) -> list_to_binary(lists:flatten(io_lib:format("~.2f",[X]))).

%% @doc universal converter to integer(list)
-spec to_int(binary()|list()|integer()|atom()) -> integer().
to_int(X) when is_integer(X) -> X;
to_int(X) when is_binary(X) -> list_to_integer(binary_to_list(X));
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(X) when is_float(X) -> round(X);
to_int(X) when is_atom(X) -> list_to_integer(atom_to_list(X)).

%% @doc universal converter to float
-spec to_float(binary()|list()|float()) -> float().
to_float(X) when is_float(X) ->X;
to_float(X) when is_binary(X) -> ls_tf(binary_to_list(X));
to_float(X) when is_list(X) -> ls_tf(X).
ls_tf(X)-> case lists:member($., X) of true -> list_to_float(X); false -> list_to_float(X ++ ".0") end.

get_param_from_config(Opt) when is_atom(Opt) ->
  get_param_from_config(Opt, undefined).
get_param_from_config(Opt, Default) when is_atom(Opt) ->
  case application:get_env(cepb, Opt) of
    {ok, Val} -> Val;
    _ -> Default
  end.
