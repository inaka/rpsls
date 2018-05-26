%% @author Matthew Tovbin <tovbinm@gmail.com>
%% @doc Randy is another random player.
%%
%% Explanation: 
%% Similar to RPS game,  RPSLS is a balanced game, i.e. win chances distribute uniformely over all the choices. 
%% So it is impossible to gain an advantage over a truly random opponent. 
%% This can be easily seen by looking into the RPSLS normal form matrix (http://goo.gl/UTzgE), which is one of magic square
%% matrices that sums to 0 in any column/row.
%% Additionally one can easily be verify the chances of winning in each round by solving the following linear program:
%% (build from the normal form matrix)
%%
%% maximize x0 subject to:
%%          -x2 + x3 + x4 - x5 - x0 >= 0
%%      x1      - x3 - x4 + x5 - x0 >= 0
%%     -x1 + x2      + x4 - x5 - x0 >= 0
%%     -x1 + x2 - x3      + x5 - x0 >= 0
%%      x1 - x2 + x3 - x4      - x0 >= 0
%%      x1 + x2 + x3 + x4 + x5      == 1
%% where: x0,x1,x2,x3,x4,x5 >= 0
%% 
%% Which gives us an optimal solution of: (0.2, 0.2, 0.2, 0.2, 0.2), i.e.  uniform distribution.     
%%  

-module(rpsls_randy_player).
-author('Matthew Tovbin <tovbinm@gmail.com>').

-behaviour(rpsls_player).

-export([init/0, play/2]).

%% @private
-spec init() -> State::term().
init() -> {}.

-spec play(History::[{You::rpsls_player:choice(), Rival::rpsls_player:choice()}], State::term()) -> {rpsls_player:choice(), NewState::term()}.
play(_History, {}) -> {lists:nth(rand:uniform(5), [rock, paper, scissors, lizard, spock]), {}}.
