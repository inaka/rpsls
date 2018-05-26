-module(rpsls_nicolas_player).
-author('nicolas nicolasmd87@gmail.com').

-behaviour(rpsls_player).

-export([init/0, play/2]).


-spec init() -> State::term().

init() ->
 {} .

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.

play(History,State) ->
	case History of
		[]->
			{lists:nth(rand:uniform(5), [rock, paper, scissors, lizard, spock]), State};
		_Other->
			{ProcessedHistoryList, _TotalPlayedQuantity}=processHistory(History,[{rock,0},{paper,0},{scissors,0},{lizard,0},{spock,0}],0),
			%%Ordeno la lista de mayor a menor
			ProcessedHistoryList_Sorted=lists:keysort(2,ProcessedHistoryList),
			%%Ahora se la jugada mas utilizada por mi rival (Key), pero primero deberia verificar si esa jugada ya la jugo la mano pasada, eso me daria un indicio que capaz eliga la segunda mas jugada por el
			%%No es el algoritmo mas ganador del mundo pero sirve
			{Key,_Val}=lists:last(ProcessedHistoryList_Sorted),
			[{_,LastRivalPlay}|_RestOfHistory]=History,
			WinningPlayKey=if(Key==LastRivalPlay)->
								 {NewKey,_}=lists:last(ProcessedHistoryList_Sorted),
								 getWinningKeyFor(NewKey);
							 true->
								 getWinningKeyFor(Key)
						   end,
			{WinningPlayKey,State}
	end.

getWinningKeyFor(rock) -> paper;
getWinningKeyFor(paper) -> scissors;
getWinningKeyFor(scissors) -> rock;
getWinningKeyFor(lizard) -> scissors;
getWinningKeyFor(spock) -> lizard.


%%Se procesa el historial recursivamente para saber cuantas ocurrencias de cada jugada del rival hay para su posterior procesamiento
processHistory([],ProcessedHistoryList,PlayCounter)->
	{ProcessedHistoryList,PlayCounter};

processHistory(History,ProcessedHistoryList,PlayCounter)->
	[{_,RivalPlay}|RestOfHistory]=History,
	NewProcessedHistoryList=case RivalPlay of
								rock->
									generateNewProcessedList(rock,ProcessedHistoryList);
								paper->
									generateNewProcessedList(paper,ProcessedHistoryList);
								scissors->
									generateNewProcessedList(scissors,ProcessedHistoryList);
								lizard->
									generateNewProcessedList(lizard,ProcessedHistoryList);
								spock->
									generateNewProcessedList(spock,ProcessedHistoryList)
							end,
	processHistory(RestOfHistory,NewProcessedHistoryList,PlayCounter+1).

%%Busca por clave (rock,paper,etc) y luego incrementa en su debido contador y devuelve la lista
generateNewProcessedList(Key,ProcessedHistoryList)->
	{_,{TupleKey,Value}}=lists:keysearch(Key,1,ProcessedHistoryList),
	lists:keyreplace(TupleKey,1,ProcessedHistoryList,{TupleKey,Value+1}).


