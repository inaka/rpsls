-module(rpsls_nicolas_player).
-author('nicolas nicolasmd87@gmail.com').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> []. 

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.

play(_, State) -> {lists:nth(random:uniform(2), [rock, paper]),State}.
