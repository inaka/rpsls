%% @author Leor Leor <leorleor@gmail.com>
%% @copyright 2012
%% @doc Rockem Spockem bot by Leor
-module(rpsls_leor_player).
-author(' Leor Leor <leorleor@gmail.com>').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> _ = random:seed(erlang:now()), {}.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play(_History, {}) -> {lists:nth(random:uniform(5), [paper, scissors, spock, rock, lizard]), {}}.
