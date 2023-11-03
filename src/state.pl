/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */



% Declaring dynamic variables as so.
:- dynamic current_player/1.
:- dynamic current_game_state/1.
:- dynamic winner/1.

% Basic game states.
state(menu).
state(placement_phase).
state(movement_phase).
state(end_game).
state(white_wins).
state(black_wins).

% Facts defining both players.
player(white).
player(black).

% Dynamic facts to determine in-game logic.
current_game_state(menu).
current_player(white).
winner(_).

% Facts to determine player switching.
player_switch(white, black).
player_switch(black, white).

% Facts to determine state switching.
state_switch(menu, placement_phase).
state_switch(placement_phase, movement_phase).
state_switch(movement_phase, end_game).

% Facts to determine game winner.
state_win_decider(end_game, white, white_wins).
state_win_decider(end_game, black, black_wins).

% Rule to evaluate the existance of a winner.
state_win_checker :-
        current_game_state(CurrentState),
        current_player(CurrentPlayer),
        state_win_decider(CurrentState, CurrentPlayer, FinalState),
        retract(winner(_)),
        retract(current_game_state(CurrentState)),
        asserta(current_game_state(FinalState)),
        asserta(winner(CurrentPlayer)).

state_win_checker.

% Rule to switch the current game state.
state_switch_forward :-
        current_game_state(CurrentState),
        state_switch(CurrentState, New),
        retract(current_game_state(CurrentState)),
        asserta(current_game_state(New)).

state_switch_forward.

% Rule to switch between players.
player_switcher :-
        current_player(CurrentPlayer),
        player_switch(CurrentPlayer, NewPlayer),
        retract(current_player(CurrentPlayer)),
        asserta(current_player(NewPlayer)).

player_switcher.

% Rule to force set the current player.
player_setter(SelectedPlayer) :-        % Functional only once, at the beggin of game, to determine which player starts first.
        player(SelectedPlayer),
        current_player(CurrentPlayer),
        retract(current_player(CurrentPlayer)),
        asserta(current_player(SelectedPlayer)).        

player_setter(_).

% Rule for abstract state control (Player switching, win checking).
state_control :-
        state_win_checker,
        player_switcher.
      

        

