/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */


state(menu).
state(placement_phase).
state(movement_phase).
state(win_condition_met).
state(white_wins).
state(black_wins).

player(white).
player(black).

current_game_state(menu).
current_player_state(white).
winner(_).

player_switch(white, black).
player_switch(black, white).

state_switch(menu, placement_phase).
state_switch(placement_phase, movement_phase).
state_switch(movement_phase, win_condition_met).

state_win_decider(win_condition_met, white, white_wins).
state_win_decider(win_condition_met, black, black_wins).

state_win_checker(CurrentState, CurrentPlayer) :-
        state_win_decider(CurrentState, CurrentPlayer, FinalState).
        % Something about setting the rule "winner" to the "CurrentPlayer" player.

state_win_checker(_,_).

state_switcher(CurrentState) :-
        state_switch(CurrentState, New).
        % Something about setting the rule "current_state" to "New".

state_switcher(_).

player_switcher(CurrentPlayer) :-
        player_switch(CurrentPlayer, NewPlayer).
        % Something about setting the rule "current_player_state" to "NewPlayer".

player_switcher(_).

player_setter(SelectedPlayer) :-        % Functional only once, at the beggin of game, to determine which player starts first.
        player(SelectedPlayer).
        % Something about setting the rule "current_player_state" to "SelectedPlayer".

player_setter(_).

        

