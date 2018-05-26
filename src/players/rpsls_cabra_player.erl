%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2014, Carlos Andres Bolaños R.A.
%%% @doc
%%% This player detects predictable patterns such as: alternating
%%% pattern, repeating pattern (like bart, spock) and copycat pattern.
%%% In other cases the logic applied is: if the opponent wins, then
%%% pick the thing that beats what the opponent just played. If my
%%% player wins, then pick the thing that beats the possible things
%%% which can beat the thing that my player just played.
%%% The main idea is obtaining a significant advantage over those
%%% predictable pattern, and to unpredictable patterns like random
%%% and others, try to be as much balanced as possible
%%% @end
%%% Created : 13. Dec 2014 12:02 PM
%%%-------------------------------------------------------------------
-module(rpsls_cabra_player).

-behaviour(rpsls_player).

%% API
-export([init/0, play/2]).

%% Possible and valid choices
-define(CHOICES, [rock, paper, scissors, lizard, spock]).

%%%===================================================================
%%% API
%%%===================================================================

%% @private
-spec init() -> State::term().
init() ->
    {}.

%% @doc
%% 1. First case in the random initialization
%% 2. Match Alternating pattern
%% 3. Match Repetitive pattern (bart, spock, ..)
%% 4. Match Copycat pattern
%% 5. Default behavior:
%% This applies to both cases. In case to lose, applying
%% "who_defeat(who_defeat(Me))", the first result from nested call returns
%% possible things that can defeat me, including the opponent thing, and then
%% applying again the same function over the returned result, will give me
%% the thing that can defeat them. In case of win, well this case is pretty
%% much obvious.
-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}],
           State::term()) -> {rpsls_player:choice(), NewState::term()}.
play([], State) ->
    {lists:nth(rand:uniform(5), ?CHOICES), State};
play([{_, Rival}, {_, _}, {_, _}, {_, _}, {_, Rival0}, {_, Rival}|_T], State) ->
    {pick_one(who_defeat(Rival0)), State};
play([{_, Rival}, {_, Rival}, {_, Rival}|_T], State) ->
    {pick_one(who_defeat(Rival)), State};
play([{Me0, Me1}, {Me1, Me2}, {Me2, _}|_T], State) ->
    {pick_one(who_defeat(Me0)), State};
play([{Me, _Rival}|_T], State) ->
    {who_defeat(who_defeat(Me)), State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
pick_one(L) -> lists:nth(rand:uniform(2), L).

%% @private
who_defeat(scissors)           -> [spock, rock];
who_defeat(paper)              -> [scissors, lizard];
who_defeat(rock)               -> [paper, spock];
who_defeat(lizard)             -> [scissors, rock];
who_defeat(spock)              -> [lizard, paper];
who_defeat([spock, rock])      -> paper;
who_defeat([rock, spock])      -> paper;
who_defeat([scissors, lizard]) -> rock;
who_defeat([lizard, scissors]) -> rock;
who_defeat([paper, spock])     -> lizard;
who_defeat([spock, paper])     -> lizard;
who_defeat([scissors, rock])   -> spock;
who_defeat([rock, scissors])   -> spock;
who_defeat([lizard, paper])    -> scissors;
who_defeat([paper, lizard])    -> scissors.
