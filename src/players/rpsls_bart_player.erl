-module(rpsls_bart_player).
-author('joachim joachim@inaka.net').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> []. %%_ = random:seed(erlang:now()), {}.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play(_, State) -> {rock, State}.
