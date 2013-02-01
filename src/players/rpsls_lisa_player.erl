%%Lisa plays 50% of spock and the rest equally divided over the other options.

-module(rpsls_lisa_player).
-author('Joachim Nilsson <joachim@inakanetworks.com>').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> _ = random:seed(erlang:now()), {}.


generate(1) -> rock;
generate(2) -> paper;
generate(3) -> scissors;
generate(4) -> lizard;
generate(_) -> spock.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play(_History, {}) -> {generate(random:uniform(8)), {}}.