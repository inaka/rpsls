-module (rpsls_bart_player_SUITE).
-export([all/0, test/1, test2/1, test3/1, test4/1]).

all() ->
  [test, test2, test3, test4].

test(_) -> {draw, _, [{rock,rock}]} = rpsls:play(rpsls_bart_player,rpsls_bart_player,1).

test2(_) -> {draw, _, [{rock,rock}, {rock,rock}]} = rpsls:play(rpsls_bart_player,rpsls_bart_player,2).

test3(_) -> {draw, _, [{rock,rock}, {rock,rock}, {rock,rock}]} = rpsls:play(rpsls_bart_player,rpsls_bart_player,3).

test4(_) -> {draw, _, [{rock,rock}, {rock,rock}, {rock,rock}, {rock,rock}]} = rpsls:play(rpsls_bart_player,rpsls_bart_player,4).




