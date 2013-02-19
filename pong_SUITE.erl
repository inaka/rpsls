-module(pong_SUITE).
-export([run_wins/2, all/0, test1/1, test2/1, test3/1, test4a/1, test4b/1, test5a/1, test5b/1, test6a/1, test6b/1, test7a/1, test7b/1, test8a/1, test8b/1, test9a/1, test9b/1]).
 
 %%Start the pong FSM with pong:start_link(). and run this test module directly afterwards, so that the pong FSM is in its' initial untouched state.

all() ->
  [test1, test2, test3, test4a, test4b, test5a, test5b, test6a, test6b, test7a, test7b, test8a, test8b, test9a, test9b].

test1(_) -> 
  pong:start_link(),
  [{game_status,[on_course,{a,0},{b,0}]}, {current_state,b_right}] = pong:score().

test2(_) -> 
  pong:stop(),
  pong:start_link(),
  pong:win(a),
  [{game_status,[on_course,{a,1},{b,0}]}, {current_state,a_left}] = pong:score().

test3(_) -> 
  pong:stop(),
  pong:start_link(),
  pong:win(a),
  pong:win(b),
  [{game_status,[on_course,{a,1},{b,1}]}, {current_state,b_right}] = pong:score().

test4a(_) ->
  pong:stop(),
  pong:start_link(),
  pong:win(b),
  [{game_status,[game_over,{a,11},{b,1}]}, {current_state,game_over}] = run_wins(11,a).

test4b(_) ->
  pong:stop(),
  pong:start_link(),
  pong:win(a),
  [{game_status,[game_over,{a,1},{b,11}]}, {current_state,game_over}] = run_wins(11,b).

test5a(_) -> 
  pong:stop(),
  pong:start_link(),
  [{game_status,[game_over,{a,7},{b,0}]}, {current_state,game_over}] = run_wins(7,a).

test5b(_) -> 
  pong:stop(),
  pong:start_link(),
  [{game_status,[game_over,{a,0},{b,7}]}, {current_state,game_over}] = run_wins(7,b).

test6a(_) -> 
  pong:stop(),
  pong:start_link(),
  run_wins(2,b),
  [{game_status,[game_over,{a,21},{b,2}]}, {current_state,game_over}] = run_wins(21,a).

test6b(_) ->
  pong:stop(),
  pong:start_link(),
  run_wins(2,a),
  [{game_status,[game_over,{a,2},{b,21}]}, {current_state,game_over}] = run_wins(21,b).

test7a(_) ->
  pong:stop(),
  pong:start_link(),
  run_wins(2,a), %%in order not to get caught by 7-0 or 11-1 wins
  run_wins(20,b),
  [{game_status,[game_over,{a,22},{b,20}]}, {current_state,game_over}] = run_wins(20,a).

test7b(_) ->
  pong:stop(),
  pong:start_link(),
  run_wins(2,b), %%in order not to get caught by 7-0 or 11-1 wins
  run_wins(20,a),
  [{game_status,[game_over,{a,20},{b,22}]}, {current_state,game_over}] = run_wins(20,b).

test8a(_) ->
  pong:stop(),
  pong:start_link(),
  %%in order not to get caught by 7-0 or 11-1 or X-21, X<20 wins
  run_wins(3,b),
  run_wins(20,a), 
  run_wins(18,b),
  run_wins(2,a), 
  alternate_wins(3,b),
  [{game_status,[game_over,{a,25},{b,23}]}, {current_state,game_over}] = run_wins(2,a).

test8b(_) ->
  pong:stop(),
  pong:start_link(),
  %%in order not to get caught by 7-0 or 11-1 or X-21, X<20 wins
  run_wins(3,a),
  run_wins(20,b), 
  run_wins(18,a),
  run_wins(2,b), 
  alternate_wins(3,a),
  [{game_status,[game_over,{a,23},{b,25}]}, {current_state,game_over}] = run_wins(2,b).

test9a(_) ->
  pong:stop(),
  pong:start_link(),
  %%in order not to get caught by 7-0 or 11-1 or X-21, X<20 or Y+2 - Y wins
  run_wins(3,b),
  run_wins(20,a), 
  run_wins(18,b),
  run_wins(2,a), 
  [{game_status,[game_over,{a,51},{b,50}]}, {current_state,game_over}] = alternate_wins(58,b).

test9b(_) ->
  pong:stop(),
  pong:start_link(),
  %%in order not to get caught by 7-0 or 11-1 or X-21, X<20 wins
  run_wins(3,a),
  run_wins(20,b), 
  run_wins(18,a),
  run_wins(2,b), 
  [{game_status,[game_over,{a,50},{b,51}]}, {current_state,game_over}] = alternate_wins(58,a).


run_wins(0,_) -> pong:score();
run_wins(N,Who) -> pong:win(Who), run_wins(N-1, Who).

alternate_wins(0,_) -> pong:score();
alternate_wins(N,a) -> pong:win(a), alternate_wins(N-1,b);
alternate_wins(N,b) -> pong:win(b), alternate_wins(N-1,a).
