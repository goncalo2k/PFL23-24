/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */


% Basic game states.
state(menu).
state(placement_phase).
state(movement_phase).
state(win_condition_met).
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
state_switch(movement_phase, win_condition_met).

% Facts to determine game winner.
state_win_decider(win_condition_met, white, white_wins).
state_win_decider(win_condition_met, black, black_wins).

% Rule to evaluate the existance of a winner.
state_win_checker(CurrentState, CurrentPlayer) :-
        state(CurrentState),
        player(CurrentPlayer),
        state_win_decider(CurrentState, CurrentPlayer, FinalState).
        % Something about setting the fact "winner" to the "CurrentPlayer" player.

state_win_checker(_,_).

% Rule to switch the current game state.
state_switcher(CurrentState) :-
        state(CurrentState),
        state_switch(CurrentState, New).
        % Something about setting the fact "current_state" to "New".

state_switcher(_).

% Rule to switch between players.
player_switcher(CurrentPlayer) :-
        player(CurrentPlayer),
        player_switch(CurrentPlayer, NewPlayer).
        % Something about setting the fact "current_player " to "NewPlayer".

player_switcher(_).

% Rule to force set the current player.
player_setter(SelectedPlayer) :-        % Functional only once, at the beggin of game, to determine which player starts first.
        player(SelectedPlayer).
        % Something about setting the fact "current_player " to "SelectedPlayer".

player_setter(_).

% Rule for abstract state control (Player switching, win checking).
state_control :-
        current_player(Player),
        current_game_state(State),
        state_win_checker(State, Player),
        player_switcher(Player).
      

        

