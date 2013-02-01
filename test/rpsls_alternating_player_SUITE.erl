-module (rpsls_alternating_player_SUITE).
-export([all/0, test1/1, test2/1, test3/1, test4/1, test5/1, test10/1]).

all() ->
  [test1, test2, test3, test4, test5, test10].

test1(_) -> {draw, _, [{rock,rock}]} = rpsls:play(rpsls_alternating_player,rpsls_alternating_player,1).
test2(_) -> {draw, _, [{paper,paper}, {rock,rock}]} = rpsls:play(rpsls_alternating_player,rpsls_alternating_player,2).
test3(_) -> {draw, _, [{scissors,scissors}, {paper,paper}, {rock,rock}]} = rpsls:play(rpsls_alternating_player,rpsls_alternating_player,3).
test4(_) -> {draw, _, [{lizard,lizard}, {scissors,scissors}, {paper,paper}, {rock,rock}]} = rpsls:play(rpsls_alternating_player,rpsls_alternating_player,4).
test5(_) -> {draw, _, [{spock,spock}, {lizard,lizard}, {scissors,scissors}, {paper,paper}, {rock,rock}]} = rpsls:play(rpsls_alternating_player,rpsls_alternating_player,5).
test10(_) -> {draw, _, [{spock,spock}, {lizard,lizard}, {scissors,scissors}, {paper,paper}, {rock,rock}, {spock,spock}, {lizard,lizard}, {scissors,scissors}, {paper,paper}, {rock,rock}]} = rpsls:play(rpsls_alternating_player,rpsls_alternating_player,10).


