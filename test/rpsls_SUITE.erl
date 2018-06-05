-module(rpsls_SUITE).

-export([ all/0
        , start_stop/1
        , players/1
        , all_combos/1
        , p1_win/1
        , p2_win/1
        , invalid/1
        , failure/1
        , both_disqualified/1
        ]).

all() -> [start_stop, players, all_combos, p1_win, p2_win, invalid, failure, both_disqualified].

start_stop(_) ->
  {ok, _} = rpsls:start(),
  ok = application:stop(rpsls),
  {comment, ""}.

players(_) ->
  Players = rpsls:players(),
  true = lists:member(rpsls_bart_player, Players),
  {comment, ""}.

%% @doc The idea is to play a long enough game, with known players to verify all options
all_combos(_) ->
  {ok, _} = rpsls:start(),
  {draw, 40, _} = rpsls:play(rpsls_rotating_player, rpsls_rotating_five_player, 100),
  {comment, ""}.

p1_win(_) ->
  {ok, _} = rpsls:start(),
  {rpsls_rotating_five_player, 1, 0, [{spock, rock}]} =
    rpsls:play(rpsls_rotating_five_player, rpsls_rotating_player),
  {comment, ""}.

p2_win(_) ->
  {ok, _} = rpsls:start(),
  {rpsls_rotating_five_player, 1, 0, [{rock, spock}]} =
    rpsls:play(rpsls_rotating_player, rpsls_rotating_five_player),
  {comment, ""}.

invalid(_) ->
  {ok, _} = rpsls:start(),
  {rpsls_rotating_player, rival_disqualified} =
    rpsls:play(rpsls_rotating_player, rpsls_invalid_player),
  {rpsls_rotating_player, rival_disqualified} =
    rpsls:play(rpsls_invalid_player, rpsls_rotating_player),
  {comment, ""}.

failure(_) ->
  {ok, _} = rpsls:start(),
  {rpsls_rotating_player, rival_disqualified} =
    rpsls:play(rpsls_rotating_player, rpsls_fail_player),
  {rpsls_rotating_player, rival_disqualified} =
    rpsls:play(rpsls_fail_player, rpsls_rotating_player),
  {comment, ""}.

both_disqualified(_) ->
  {ok, _} = rpsls:start(),
  {draw, both_disqualified} = rpsls:play(rpsls_invalid_player, rpsls_fail_player),
  {comment, ""}.
