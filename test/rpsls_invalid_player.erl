%%% @doc Returns invalid results
-module(rpsls_invalid_player).

-dialyzer([{nowarn_function, play/2}]).

-behaviour(rpsls_player).

-export([init/0, play/2]).

init() -> {}.

play(_History, {}) -> {invalid, {}}.
