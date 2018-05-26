%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright 2012 Inaka Networks
%% @doc Player that always choose spock
-module(rpsls_marcelo_player).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> spock.

choose_winner(spock) ->
	lizard;

choose_winner(lizard) ->
	rock;

choose_winner(rock) ->
	paper;

choose_winner(paper) ->
	scissors;

choose_winner(scissors) ->
	spock.

-spec play(rpsls_player:history(), rpsls_player:state()) -> {rpsls_player:choice(), NewState::term()}.
play(History, _State) ->
	MoveCount = lists:foldl(
		fun(Round, Acc) ->
			{_Mine, Opponent} = Round,
			{Opponent,Times} = lists:keyfind(Opponent, 1, Acc),
			lists:keystore(Opponent, 1, Acc, {Opponent, Times + 1})
		end,
		[{spock, 0}, {scissors, 0}, {paper, 0}, {rock, 0}, {lizard, 0}],
		History
	),
	{Move, _Times} = lists:last(lists:keysort(2, MoveCount)),
	{choose_winner(Move), _State}.
