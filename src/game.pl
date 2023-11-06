/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */
piece_percentage(37).

display_game(GameState) :-
        length(GameState, Length),
        display_game(GameState, Length).

start_game(P1, P2) :-
    write('What is your desired board size? Must be an odd number. Default is 9, Minimum is 5 and Maximum is 15.'), nl,
    read(Temp),
    verify_input(Temp, 5, 16, Size),
    initial_state(Size,Board),
    get_board_length(Board, Total),
    piece_percentage(Percetage),
    HalfPieceCount is (Total*Percetage) // 200,
    display_game(Board),
    state_switch_forward,
    placement_phase_loop(Board,Size, HalfPieceCount).
    %movement_phase_loop(Board,Size).

movement_phase_loop(Board,Size) :-
    display_game(Board,Size),
    read_move(X1,Y1,X2,Y2),
    move_piece(X1,Y1,X2,Y2,Board,NewBoard),
    (win_conditioning_check(NewBoard) -> write('Theres a winner!');
    movement_phase_loop(NewBoard,Size)).

%Placement Phase
placement_phase_loop(Board, Size, 0) :-
    movement_phase_loop(Board,Size).
placement_phase_loop(Board,Size, N) :-
    TempN is N*2,
    write('Missing '), write(TempN), write(' pieces on the board.'),
    current_player(Player),
    (Player == black -> write('Black ') ; write('White ')),
    write('player, what is the color of the piece you want to place?'),
    nl,
    read(Color),
    manage_color_input(Board,Size, N, Color),
    write('Where would you like to place your piece?'), nl,
    coordenates_input(X, Y, Size),
    place_piece(X,Y,Board,Size,Color,NewBoard),
    display_game(NewBoard, Size),
    (Color == w -> NewColor = b; NewColor = w),
    (NewColor == b ->
        write('Now you\'re placing a piece of black colour');
        write('Now you\'re placing a piece of white colour')), nl,
    write('Where would you like to place your piece?'), nl,
    coordenates_input(X1, Y1, Size),
    place_piece(X1,Y1,NewBoard,Size,NewColor,LastBoard),
    display_game(LastBoard),
    player_switcher,
    N1 is N - 1,
    placement_phase_loop(LastBoard,Size, N1).

win_conditioning_check(Board) :-
        check_win(Board, 1);
        check_win(Board, 2).

check_win(Board, Colour) :-
        check_each_row(Board, Colour);
        check_diagonals(Board, Colour).
      