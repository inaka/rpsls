%%% @doc Plays 5 times each option in round-robin, starting with rock
-module(rpsls_rotating_five_player).

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> {}.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) ->
  {rpsls_player:choice(), NewState::term()}.
play(History, {}) -> {choose(length(History) rem 25), {}}.

choose(X) when  0 =< X, X =< 4  -> spock;
choose(X) when  5 =< X, X =< 9  -> rock;
choose(X) when 10 =< X, X =< 14 -> paper;
choose(X) when 15 =< X, X =< 19 -> scissors;
choose(X) when 20 =< X, X =< 24 -> lizard.
