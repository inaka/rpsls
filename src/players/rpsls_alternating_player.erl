%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright 2012 Inaka Networks
%% @doc Player that goes round robin around the choices
-module(rpsls_alternating_player).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> [rock, paper, scissors, lizard, spock].

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play(_History, [Choice|Rest]) -> {Choice, Rest ++ [Choice]}.