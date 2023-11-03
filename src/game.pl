/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

start_game(P1, P2) :-
    write('What is your desired board size? (default is 9)'), nl,
    read(Size),
    initialize_board(Board,Size).
    %display_game(Board,Size).