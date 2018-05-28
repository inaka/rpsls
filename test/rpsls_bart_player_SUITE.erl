-module (rpsls_bart_player_SUITE).

-export([all/0, test/1]).

all() -> [test].

test(_) ->
  {draw, _, History} = rpsls:play(rpsls_bart_player, rpsls_bart_player, rand:uniform(1000) + 1),
  [{rock, rock}] = lists:usort(History).
