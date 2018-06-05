%%% @doc Plays round-robin, starting with rock
-module(rpsls_rotating_player).

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> {}.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) ->
  {rpsls_player:choice(), NewState::term()}.
play(History, {}) -> {choose(length(History) rem 5), {}}.

choose(0) -> rock;
choose(1) -> paper;
choose(2) -> scissors;
choose(3) -> lizard;
choose(4) -> spock.
