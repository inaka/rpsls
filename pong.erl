%%%-------------------------------------------------------------------
%%% File    : pong.erl
%%% Author  : Joachim Nilsson <joachim@inakanetworks.com>
%%% Description : FSM to track inakapong matches given iformation of each turn it will give who servs from where and %%%the score.
%%%
%%% Created :  13 Feb 2013 
%%%-------------------------------------------------------------------
-module(pong).

-behaviour(gen_fsm).

%% API
-export([start_link/0, win/1, score/0, stop/0]).

%% gen_fsm callbacks
-export([init/1, state_name/2, a_left/3, a_right/3, b_left/3, b_right/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4, game_over/3,verify_score/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------

start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).
  %the console will be a process that is linked to this server, so if one dies by exception then both dies


%%Call win(X) to register a score where X is the scoring player, a or b. Any other input will give the current score.
win(X) -> 
  gen_fsm:sync_send_event(?SERVER, X).

score() ->
  gen_fsm:sync_send_all_state_event(?SERVER, count_score).

stop() ->
  gen_fsm:sync_send_all_state_event(?SERVER, stop).





%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([]) ->
  io:format("First player is b_right!~n"),
  {ok, b_right, [on_course, {a, 0}, {b, 0}]}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%--------------------------------------------------------------------

state_name(Event, State) ->
  {next_state, Event, State}.


%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------

game_over(_Event, _From, State) -> {reply, State, game_over, State}.

b_right(a, _From, [on_course, {a, Score_a}, {b, Score_b}]) ->
  State = verify_score([on_course, {a, Score_a + 1}, {b, Score_b}]),
  case hd(State) of 
    on_course -> 
      NextStateName = a_left, 
      Reply = "It's time for player a_left to serve!";
    game_over -> 
      NextStateName = game_over, 
      Reply = "We have a winner!"

    end,
  {reply, Reply, NextStateName, State};

b_right(b, _From, [on_course, {a, Score_a}, {b, Score_b}]) ->
  State = verify_score([on_course, {a, Score_a}, {b, Score_b + 1}]),
  case hd(State) of
    on_course -> 
      NextStateName = b_left, 
      Reply = "It's time for player b_left to serve!";
    game_over ->   
      NextStateName = game_over, 
      Reply = "We have a winner!"
    end,
  {reply, Reply, NextStateName, State};

b_right(_, _From, State) ->
  io:format("Enter pong:win(a). or pong:win(b). to register a win for the respective player. Current game status:~n"),
  {reply, State, b_left, State}.



b_left(a, _From, [on_course, {a, Score_a}, {b, Score_b}]) ->
  State = verify_score([on_course, {a, Score_a + 1}, {b, Score_b}]),
  case hd(State) of
    on_course -> 
      NextStateName = a_right, 
      Reply = "It's time for player a_right to serve!";
    game_over ->   
      NextStateName = game_over, 
      Reply = "We have a winner!"
    end,
  {reply, Reply, NextStateName, State};

b_left(b, _From, [on_course, {a, Score_a}, {b, Score_b}]) ->
  State = verify_score([on_course, {a, Score_a}, {b, Score_b + 1}]),
  case hd(State) of
    on_course -> 
      NextStateName = b_right, 
      Reply = "It's time for player b_right to serve!";
    game_over ->   
      NextStateName = game_over, 
      Reply = "We have a winner!"
    end,
  {reply, Reply, NextStateName, State};

b_left(_, _From, State) ->
  io:format("Enter pong:win(a). or pong:win(b). to register a win for the respective player. Current game status:~n"),
  {reply, State, b_left, State}.



a_right(a, _From, [on_course, {a, Score_a}, {b, Score_b}]) ->
  State = verify_score([on_course, {a, Score_a + 1}, {b, Score_b}]),
  case hd(State) of
    on_course -> 
      NextStateName = a_left, 
      Reply = "It's time for player a_left to serve!";
    game_over ->   
      NextStateName = game_over, 
      Reply = "We have a winner!"
  end,
  {reply, Reply, NextStateName, State};

a_right(b, _From, [on_course, {a, Score_a}, {b, Score_b}]) ->
 State = verify_score([on_course, {a, Score_a}, {b, Score_b + 1}]),
  case hd(State) of 
    on_course -> 
      NextStateName = b_left, 
      Reply = "It's time for player b_left to serve!";
    game_over ->   
      NextStateName = game_over, 
      Reply = "We have a winner!"
  end,
  {reply, Reply, NextStateName, State};

a_right(_, _From, State) ->
  io:format("Enter pong:win(a). or pong:win(b). to register a win for the respective player. Current game status:~n"),
  {reply, State, b_left, State}.



a_left(a, _From, [on_course, {a, Score_a}, {b, Score_b}]) ->
  State = verify_score([on_course, {a, Score_a + 1}, {b, Score_b}]),
  case hd(State) of
    on_course -> 
      NextStateName = a_right, 
      Reply = "It's time for player a_right to serve!";
    game_over ->   
      NextStateName = game_over, 
      Reply = "We have a winner!"
  end,
  {reply, Reply, NextStateName, State};


a_left(b, _From, [on_course, {a, Score_a}, {b, Score_b}]) ->
  State = verify_score([on_course, {a, Score_a}, {b, Score_b + 1}]),
  case hd(State) of
    on_course -> 
      NextStateName = b_right, 
      Reply = "It's time for player b_right to serve!";
    game_over ->   
      NextStateName = game_over, 
      Reply = "We have a winner!"
  end,
  {reply, Reply, NextStateName, State};

a_left(_, _From, State) ->
  io:format("Enter pong:win(a). or pong:win(b). to register a win for the respective player. Current game status:~n"),
  {reply, State, b_left, State}.



%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                                                NextState} |
%%                                          {next_state, NextStateName,
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.



%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(stop, _From, _StateName, State) -> {stop, normal, ok, State};


handle_sync_event(count_score, _From, StateName, State) ->
  Both_states = [{game_status, State}, {current_state, StateName}],
  {reply, Both_states, StateName, State}.


%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

verify_score([on_course, {a, 7}, {b, 0}]) -> [game_over, {a, 7}, {b, 0}];
verify_score([on_course, {a, 0}, {b, 7}]) -> [game_over, {a, 0}, {b, 7}];

verify_score([on_course, {a, 11}, {b, 1}]) -> [game_over, {a, 11}, {b, 1}];
verify_score([on_course, {a, 1}, {b, 11}]) -> [game_over, {a, 1}, {b, 11}];

verify_score([on_course, {a, 51}, {b, 50}]) -> [game_over, {a, 51}, {b, 50}];
verify_score([on_course, {a, 50}, {b, 51}]) -> [game_over, {a, 50}, {b, 51}];

verify_score([on_course, {a, 21}, {b, Score_b}]) when Score_b < 20 -> [game_over, {a, 21}, {b, Score_b}];
verify_score([on_course, {a, Score_a}, {b, 21}]) when Score_a < 20 -> [game_over, {a, Score_a}, {b, 21}];

verify_score([on_course, {a, Score_a}, {b, Score_b}]) when Score_a >= 21, Score_a >= (Score_b + 2) -> [game_over, {a, Score_a}, {b, Score_b}];
verify_score([on_course, {a, Score_a}, {b, Score_b}]) when Score_b >= 21, Score_b >= (Score_a + 2) -> [game_over, {a, Score_a}, {b, Score_b}];

verify_score(X) -> X.



