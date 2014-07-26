-module(rpsls_higherbeat_player).
-author('George Ye <yegh98@yahoo.com>').
-author('Elian Gidoni <elian@braceloop.com').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> _ = random:seed(erlang:now()), {}.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play([], State) -> {lists:nth(random:uniform(5), [rock, paper, scissors, lizard, spock]), State};
play([{Last, Mine},{Mine, _}|_], State) -> {beat_copycat(Last), State};
play([{_, Repeat},{_, Repeat }, {_, Repeat}|_], State) -> {beat_copycat(Repeat), State};
play([{Last,_}|_], State) -> {beat_winnersof(Last), State}.

%beat_winnersof(rock) -> lizard;
beat_winnersof(rock) -> lists:nth(random:uniform(2), [lizard, scissors]);

%beat_winnersof(paper) -> rock;
beat_winnersof(paper) -> lists:nth(random:uniform(2), [rock, spock]);

%beat_winnersof(scissors) -> paper;
beat_winnersof(scissors) -> lists:nth(random:uniform(2), [paper, lizard]);

%beat_winnersof(lizard) -> spock;
beat_winnersof(lizard) -> lists:nth(random:uniform(2), [spock, paper]);

%beat_winnersof(spock) -> scissors;
beat_winnersof(spock) -> lists:nth(random:uniform(2), [scissors, rock]);

beat_winnersof(_) -> scissors. %% cannot happen but give scissors as default

beat_copycat(rock) -> lists:nth(random:uniform(2), [paper, spock]);
beat_copycat(paper) -> lists:nth(random:uniform(2), [scissors, lizard]);
beat_copycat(scissors) -> lists:nth(random:uniform(2), [rock, spock]);
beat_copycat(lizard) -> lists:nth(random:uniform(2), [scissors, rock]);
beat_copycat(spock) -> lists:nth(random:uniform(2), [paper, lizard]);
beat_copycat(_) -> scissors.

