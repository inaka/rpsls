%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright 2012 Inaka Networks
%% @doc Rock Paper Scissors Lizzard Spock
-module(rpsls).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-define(is_choice(C), C == rock; C == paper; C == scissors; C == lizard; C == spock).

-record(state, {mod1    :: atom(),
                mod2    :: atom(),
                st1     :: term(),
                st2     :: term(),
                score1  :: non_neg_integer(),
                score2  :: non_neg_integer(),
                rounds  :: non_neg_integer(),
                history :: [{rpsls_player:choice(), rpsls_player:choice()}]}).

%% Application callbacks
-export([start/0, start/2, stop/1]).
-export([play/2, play/3]).
-export([players/0]).

%% ===================================================================
%% API (Global)
%% ===================================================================
%% @doc Starts the application
-spec start() -> {ok, [atom()]}.
start() -> application:ensure_all_started(?MODULE).

%% @doc Confronts two modules in a deadly match
-spec play(Mod1::atom(), Mod2::atom(), pos_integer()) ->
          {draw, Score::non_neg_integer() | both_disqualified} |
          {Winner::Mod1|Mod2, WinnerScore::non_neg_integer(), LoserScore::non_neg_integer()} |
          {Winner::Mod1|Mod2, rival_disqualified}.
play(Mod1, Mod2, Rounds) ->
  try
    play(
      #state{mod1 = Mod1, st1 = Mod1:init(), score1 = 0,
             mod2 = Mod2, st2 = Mod2:init(), score2 = 0,
             rounds = Rounds, history = []})
  catch
    _:mod1_failure ->
      {Mod2, rival_disqualified};
    _:mod2_failure ->
      {Mod1, rival_disqualified};
    _:both_failure ->
      {draw, both_disqualified}
  end.

%% @equiv play(Mod1, Mod2, 1)
-spec play(module(), module()) ->
    {module(), rival_disqualified}
  | {draw, both_disqualified}
  | {draw, non_neg_integer(), rpsls_player:history()}
  | {module(), non_neg_integer(), non_neg_integer(), rpsls_player:history()}.
play(Mod1, Mod2) -> play(Mod1, Mod2, 1).

%% @doc List all players on src/players
-spec players() -> [atom()].
players() ->
  [list_to_atom(lists:takewhile(fun(C) -> C /= $. end, Player)) || "src/players/" ++ Player <- filelib:wildcard("src/players/*.erl")].

%% ===================================================================
%% Application callbacks
%% ===================================================================
%% @private
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) -> {ok, self()}.

%% @private
-spec stop([]) -> ok.
stop([]) -> ok.

%% ===================================================================
%% Private Functions
%% ===================================================================
%% @private
play(#state{rounds = 0, score1 = Score, score2 = Score, history = History}) -> {draw, Score, History};
play(State = #state{rounds = 0}) when State#state.score1 > State#state.score2 ->
  {State#state.mod1, State#state.score1, State#state.score2, State#state.history};
play(State = #state{rounds = 0}) when State#state.score1 < State#state.score2 ->
  {State#state.mod2, State#state.score2, State#state.score1, State#state.history};
play(State) ->
  Turn1 =
    try (State#state.mod1):play(State#state.history, State#state.st1) of
      {Choice1, State1} when ?is_choice(Choice1) ->
        {Choice1, State1};
      InvalidResult1 ->
        _ = lager:error("~p return invalid result: ~p", [State#state.mod1, InvalidResult1]),
        mod1_failure
    catch
      _:Error1 ->
        _ = lager:error("~p failed: ~p", [State#state.mod1, Error1]),
        mod1_failure
    end,
  Turn2 =
    try (State#state.mod2):play([{Y,X} || {X,Y} <- State#state.history], State#state.st2) of
      {Choice2, State2} when ?is_choice(Choice2) ->
        {Choice2, State2};
      InvalidResult2 ->
        _ = lager:error("~p return invalid result: ~p", [State#state.mod2, InvalidResult2]),
        mod2_failure
    catch
      _:Error2 ->
        _ = lager:error("~p failed: ~p", [State#state.mod2, Error2]),
        mod2_failure
    end,
  case {Turn1, Turn2} of
    {mod1_failure, mod2_failure} -> throw(both_failure);
    {mod1_failure, _} -> throw(mod1_failure);
    {_, mod2_failure} -> throw(mod2_failure);        
    {{C1, S1}, {C2, S2}} ->
      NewState =
        case winner(C1, C2) of
          none -> State#state{st1 = S1, st2 = S2};
          1 -> State#state{st1 = S1, score1 = State#state.score1 + 1, st2 = S2};
          2 -> State#state{st2 = S2, score2 = State#state.score2 + 1, st1 = S1}
        end,
        _ = lager:info("~p ~p vs. ~p ~p", [NewState#state.score1, NewState#state.mod1, NewState#state.mod2, NewState#state.score2]),
        play(NewState#state{rounds = State#state.rounds - 1,
                            history = [{C1,C2} | State#state.history]})
  end.

winner(C, C) -> _ = lager:info("~p is the same as ~p", [C, C]), none;
winner(scissors, paper) -> _ = lager:info("scissors cut paper"), 1;
winner(paper, scissors) -> _ = lager:info("paper is cut by scissors"), 2;
winner(paper, rock) -> _ = lager:info("paper covers rock"), 1;
winner(rock, paper) -> _ = lager:info("rock is covered by paper"), 2;
winner(rock, lizard) -> _ = lager:info("rock crushes lizard"), 1;
winner(lizard, rock) -> _ = lager:info("lizard is crushed by rock"), 2;
winner(lizard, spock) -> _ = lager:info("lizard poisons spock"), 1;
winner(spock, lizard) -> _ = lager:info("spock is poisoned by lizard"), 2;
winner(spock, scissors) -> _ = lager:info("spock smashes scissors"), 1;
winner(scissors, spock) -> _ = lager:info("scissors are smashed by spock"), 2;
winner(scissors, lizard) -> _ = lager:info("scissors decapitate lizard"), 1;
winner(lizard, scissors) -> _ = lager:info("lizard is decapitated by scissors"), 2;
winner(lizard, paper) -> _ = lager:info("lizard eats paper"), 1;
winner(paper, lizard) -> _ = lager:info("paper is eaten by lizard"), 2;
winner(paper, spock) -> _ = lager:info("paper disproves spock"), 1;
winner(spock, paper) -> _ = lager:info("spock is disproved by paper"), 2;
winner(spock, rock) -> _ = lager:info("spock vaporizes rock"), 1;
winner(rock, spock) -> _ = lager:info("rock is vaporized by spock"), 2;
winner(rock, scissors) -> _ = lager:info("rock crushes scissors"), 1;
winner(scissors, rock) -> _ = lager:info("scissors are crushed by rock"), 2.
