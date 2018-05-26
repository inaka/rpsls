%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright 2012 Inaka Networks
%% @doc Rock Paper Scissors Lizzard Spock
-module(rpsls_player).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-type choice() :: rock | paper | scissors | lizard | spock.
-export_type([choice/0]).

-type history() :: [{You::choice(), Rival::choice()}].
-export_type([history/0]).

-type state() :: term().
-export_type([state/0]).

-callback init() -> state().
-callback play(history(), state()) -> {choice(), state()}.
