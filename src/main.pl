:-ensure_loaded('board.pl').
:-ensure_loaded('game.pl').
:-ensure_loaded('state.pl').
:-ensure_loaded('helper_predicates.pl').

main_menu_loop :-
    display_main_menu,
    read(Input),
    manage_input(Input),
    main_menu_loop.

% Rule to display the main menu.
display_main_menu :-
    nl,
    nl,
    write('===================================='), nl,
    write('         Welcome to DuAlma!         '), nl,
    write('===================================='), nl,
    write('Please select an option:            '), nl,
    write('===================================='), nl,
    write('1.       Player vs. Player          '), nl,
    write('2.       Player vs. Computer        '), nl,
    write('3.       Computer vs. Computer      '), nl,
    write('4.       Exit                       '), nl,
    write('===================================='), nl,
    write('Option: ').


manage_input(1) :-
    start_game('P','P'),
    main_menu_loop.

manage_input(2) :-
    %start_game('P','C'),
    main_menu_loop.

manage_input(3) :-
    %start_game('C','C'),
    main_menu_loop.

manage_input(4) :-
    write('Exiting...'), nl,
    exit.

manage_input(_) :-
    nl, write('Invalid option!'), nl,
    main_menu_loop.


% Rule to display the in-game menu.
% Entry point of the program.
play :-
    main_menu_loop.
