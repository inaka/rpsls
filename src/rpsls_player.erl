%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright 2012 Inaka Networks
%% @doc Rock Paper Scissors Lizzard Spock
-module(rpsls_player).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-type choice() :: rock | paper | scissors | lizard | spock.
-export_type([choice/0]).

-callback init() -> State::term().
-callback play(History::[{You::choice(), Rival::choice()}], State::term()) -> {choice(), NewState::term()}.