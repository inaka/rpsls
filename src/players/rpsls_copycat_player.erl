%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright 2012 Inaka Networks
%% @doc Player that copies its counterpart
-module(rpsls_copycat_player).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> _ = random:seed(erlang:now()), {}.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play([], {}) -> {lists:nth(random:uniform(5), [rock, paper, scissors, lizard, spock]), {}};
play([{_, Rival}|_], {}) -> {Rival, {}}.