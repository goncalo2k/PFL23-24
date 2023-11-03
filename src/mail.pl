/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */


% Define the menu options as facts.
menu_option(1, "Start Game").
menu_option(2, "Options").
menu_option(3, "Exit").

% Define game states as dynamic predicates.
:- dynamic game_state/1.
game_state(main_menu).
game_state(in_game).

% Rule to display the main menu.
display_main_menu :-
    write("Main Menu:"), nl,
    menu_option(Number, Text),
    format("~d. ~s~n", [Number, Text]),
    fail. % Backtrack to generate all menu options
display_main_menu.

% Rule to display the in-game menu.
display_in_game_menu :-
    write("In-Game Menu:"), nl,
    menu_option(Number, Text),
    format("~d. ~s~n", [Number, Text]),
    fail. % Backtrack to generate all menu options
display_in_game_menu.

% Rule to get user input and handle their choice.
get_user_choice(Choice) :-
    repeat,
    write("Enter your choice: "),
    read(Choice),
    (valid_choice(Choice) ; invalid_choice),
    !.

valid_choice(Choice) :-
    menu_option(Choice, _).

invalid_choice :-
    write("Invalid choice. Please enter a valid option."), nl,
    fail.

% Main menu loop.
main_menu_loop :-
    repeat,
    display_main_menu,
    get_user_choice(Choice),
    handle_main_menu_choice(Choice),
    Choice = 3, % Exit the loop when the user selects option 3 (Exit).
    !.

% In-game menu loop.
in_game_menu_loop :-
    repeat,
    display_in_game_menu,
    get_user_choice(Choice),
    handle_in_game_menu_choice(Choice),
    Choice = 3, % Exit the loop when the user selects option 3 (Exit).
    !.

% Rule to handle the user's choice in the main menu.
handle_main_menu_choice(1) :-
    write("Starting the game..."), nl,
    set_game_state(in_game).
handle_main_menu_choice(2) :-
    write("Options menu not implemented in this example."), nl.
handle_main_menu_choice(3) :-
    write("Goodbye!"), nl.

% Rule to handle the user's choice in the in-game menu.
handle_in_game_menu_choice(1) :-
    write("Resuming the game..."), nl,
    set_game_state(in_game).
handle_in_game_menu_choice(2) :-
    write("Options menu not implemented in this example."), nl.
handle_in_game_menu_choice(3) :-
    write("Returning to the main menu..."), nl,
    set_game_state(main_menu).

% Rule to set the current game state.
set_game_state(State) :-
    retractall(game_state(_)), % Clear all previous game states.
    assertz(game_state(State)).

% Entry point of the program.
play :-
    assertz(game_state(main_menu)), % Initialize the game state as dynamic.
    main_menu_loop.
