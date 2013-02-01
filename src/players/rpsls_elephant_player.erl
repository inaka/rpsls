%% elephant player plays remembering every previous move of the opponent and calibrates probabilities accordingly. 
%% For example, if an opponent has drawn a rock in the past, probabilities for elephant to draw spock and paper
%% increases while probabilities to draw lizard and scissors decrease.


-module(rpsls_elephant_player).
-author('Joachim Nilsson <joachim@inakanetworks.com>').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> _ = random:seed(erlang:now()), {}.



%% Increases X with 5% of the others' values (total amount of probability always has to add up to 1.00)
increase(X, List) -> %% "X" is an atom denoting a tuple in the tuple list "List"
	CalcAcc = lists:sum([V * 0.05||{Option,V} <- List, Option =/= X]),
	lists:map(fun({Option, V}) -> 
		case Option of
			X -> {X, V + CalcAcc};
			_ -> {Option, V * 0.95}
		end
	end, List).

%% Decreases X with 20% of its' own value and hands it out evenly to the others (total amount of probability always has to add up to 1.00)
decrease(X, List) -> %% "X" is an atom key denoting a tuple in the tuple list "List"
	{X, V} = lists:keyfind(X, 1, List),
	CalcAcc = V * 0.05,
	lists:map(fun({Option, K}) -> 
		case Option of
			X -> {X, K - (4 * CalcAcc)};
			_ -> {Option, K + CalcAcc}
		end
	end, List).



%%Updating probabilities according to the rules of the game
updateMemory({_, rock}, UpdatingProbabilities) -> increase(spock, increase(paper, decrease(lizard, decrease(scissors, UpdatingProbabilities))));
updateMemory({_, lizard}, UpdatingProbabilities) -> increase(scissors, increase(rock, decrease(spock, decrease(paper, UpdatingProbabilities))));
updateMemory({_, spock}, UpdatingProbabilities) -> increase(lizard, increase(paper, decrease(scissors, decrease(rock, UpdatingProbabilities)))); 
updateMemory({_, scissors}, UpdatingProbabilities) -> increase(spock, increase(rock, decrease(lizard, decrease(paper, UpdatingProbabilities))));
updateMemory(_, UpdatingProbabilities) -> increase(scissors, increase(lizard, decrease(rock, decrease(spock, UpdatingProbabilities)))). %%Corresponds to paper


%% The history list is structured in a list of tuples such as {my move, opponent move}. 
%% This function will go through the history and calibrate the probabilities according 
%% to how the opponent has been drawing so far.
analyseHistory(Logg, Calc) ->
	lists:foldl(fun updateMemory/2, Calc, Logg).



%% LAST STEP: GENERATE RANDOM NUMBER AND ALLOCATE A DRAW TO THE PLAYER
%% Send in the probabilities and send out an atom

draw(Logg, Options) -> 

	Random = random:uniform(),

	Probabilities = analyseHistory(Logg, Options),

	{rock, LimitRock} = lists:keyfind(rock, 1, Probabilities),
	{paper, ProbPaper} = lists:keyfind(paper, 1, Probabilities),
	LimitPaper = LimitRock + ProbPaper,
	{scissors, ProbScissors} = lists:keyfind(scissors, 1, Probabilities),
	LimitScissors = LimitPaper + ProbScissors,
	{lizard, ProbLizard} = lists:keyfind(lizard, 1, Probabilities),
	LimitLizard = ProbLizard + LimitScissors,

	Choose = fun(R) when R < LimitRock -> rock; 
		(R) when R < LimitPaper -> paper; 
		(R) when R < LimitScissors -> scissors;
		(R) when R < LimitLizard -> lizard;  
		(_) -> spock
	end,

	Choose(Random).





-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play(History, {}) -> {draw(History,[{spock, 0.2}, {scissors, 0.2}, {paper, 0.2}, {rock, 0.2}, {lizard, 0.2}]), {}}.



