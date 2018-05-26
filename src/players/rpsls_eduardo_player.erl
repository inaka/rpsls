%% @author Eduardo Santana <ezambomsantana@gmail.com>
%% @doc Player that always choose the most used option of the opponent. The first choise is random.
-module(rpsls_eduardo_player).
-author('Eduardo Santana').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% rpsls:play(rpsls_random_player, rpsls_eduardo_player, 5).

%% @private
-spec init() -> State::term().
init() -> {}.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play(History, State) ->
    case History of
        [] ->  {lists:nth(rand:uniform(5), [rock, paper, scissors, lizard, spock]), State};
	_ ->
            List = count_options(History, [ { rock , 0 } , { paper , 0 } , { scissors , 0 }, {lizard, 0}, {spock, 0} ]),
	    C = choose( List , 0 , rock ),
	    { C , {} }
    end.

count_options( [ Option | Options ] , Count) ->
    count_options( Options, incrementKey(element(2, Option) , Count ) );

count_options( [] , Count) ->
    Count.

choose( [ Element | Elements ] , BiggestCount , WinnerOption ) ->
    { Option , Count } = Element,
    case Count > BiggestCount of
        true -> choose( Elements , Count , Option );
        false -> choose(Elements , BiggestCount , WinnerOption )
    end;

choose( [] , _BiggestCount , WinnerOption ) ->
    WinnerOption.

incrementKey(Key, List)->
	{_, {TupleKey,Value}} = lists:keysearch(Key, 1, List),
	lists:keyreplace(TupleKey, 1, List, {TupleKey,Value+1}).

