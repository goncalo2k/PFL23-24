/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */
piece_percentage(37).

display_game(GameState) :-
        length(GameState, Length),
        display_game(GameState, Length).

start_game('P', 'P') :-
    write('What is your desired board size? Must be an odd number. Default is 9, Minimum is 5 and Maximum is 15.'), nl,
    read(Temp),
    verify_input(Temp, 5, 16, Size),
    initial_state(Size,Board),
    get_board_length(Board, Total),
    piece_percentage(Percetage),
    HalfPieceCount is (Total*Percetage) // 200,
    display_game(Board),
    state_switch_forward,
    placement_phase_loop(Board,Size, HalfPieceCount),
    winner(Winner),
    write('Congratulations! The '), write(Winner), write(' player won the game!').

movement_phase_loop(Board,Size) :-
    (game_over(Board, Winner) -> state_switch_forward, state_win_checker(Winner));
    (write('========Movement Phase========'), nl,
    display_game(Board,Size),
    current_player(Player),
    (Player == 'black' -> write('Black ') ; write('White ')),
    write('player, what is your next move?'),nl,
    read_move(X1,Y1,X2,Y2),
    (move(X1,Y1,X2,Y2,Board,NewBoard) -> movement_phase_loop(NewBoard,Size)
    ;
    movement_phase_loop(Board,Size,_)
    )).
    

movement_phase_loop(Board,Size,_) :-
        write('Error: Bad Movement '), nl,
        movement_phase_loop(Board,Size).

%Placement Phase
placement_phase_loop(Board, Size, 0) :-
    current_player(Player),
    (Player == black -> (player_switcher,write('========Movement Phase========'), nl,movement_phase_loop(Board,Size));(write('========Movement Phase========'), nl,movement_phase_loop(Board,Size))).
    
placement_phase_loop(Board,Size, N) :-
    N > 0,
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

start_game('P', 'C') :-
        write('What is your desired board size? Must be an odd number. Default is 9, Minimum is 5 and Maximum is 15.'), nl,
        read(Temp),
        verify_input(Temp, 5, 16, Size),
        initial_state(Size,Board),
        get_board_length(Board, Total),
        piece_percentage(Percetage),
        HalfPieceCount is (Total*Percetage) // 200,
        display_game(Board),
        state_switch_forward,
        write('Would you like to be the black(0) or the white pieces(1)?'), nl,
        read(Temp2),
        verify_secondary_input(Temp2, 0, 1, PColor),
        current_player(Player),
        placement_phase_loop_bot(Board,Size, HalfPieceCount,PColor).

%Placement Phase Bot
placement_phase_loop_bot(Board, Size, 0, PColor) :-
    current_player(Player),
    (Player == black -> (player_switcher,write('========Movement Phase========'), nl,display_game(Board,Size),movement_phase_loop_bot(Board,Size,PColor));(write('========Movement Phase========'), nl,display_game(Board,Size),movement_phase_loop_bot(Board,Size,PColor))).
    
placement_phase_loop_bot(Board,Size, N, PColor) :-
    (PColor == 0 -> CurPlayer = black; CurPlayer = white),
    TempN is N*2,
    write('Missing '), write(TempN), write(' pieces on the board.'),
    current_player(Player),
    write(Player),
    write(CurPlayer),
    (CurPlayer == Player ->
        ((Player == black -> write('Black ') ; write('White ')),
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
        placement_phase_loop_bot(LastBoard,Size, N1,PColor))
        ;
        (
        get_random_color(Color),
        get_random_play(Board,Size,X1,Y1),
        place_piece(X1,Y1,Board,Size,Color,NewBoard),
        display_game(NewBoard, Size),
        (Color == w -> NewColor = b; NewColor = w),
        (NewColor == b -> get_random_play(Board,Size,X1,Y1), place_piece(X2,Y2,NewBoard,Size,NewColor,LastBoard)),
        display_game(LastBoard),
        player_switcher,
        N1 is N - 1,
        placement_phase_loop_bot(LastBoard,Size, N1,PColor))
        ).


movement_phase_loop_bot(Board,Size,PColor) :-
        display_game(Board,Size),
    current_player(Player),
    (PColor == 0 -> CurPlayer = black; CurPlayer = white),
    (CurPlayer == Player ->
        ((Player == 'black' -> write('Black ') ; write('White ')),
        write('player, what is your next move?'),nl,
        read_move(X1,Y1,X2,Y2),
        (move(X1,Y1,X2,Y2,Board,NewBoard) -> movement_phase_loop_bot(NewBoard,Size,PColor)
        ;
        movement_phase_loop_bot(NewBoard,Size,PColor,_)
        ))
        ;
        (
        get_random_piece(Board,Size,X1,Y1),
        get_random_play(Board,Size,X2,Y2),
        (move(X1,Y1,X2,Y2,Board,NewBoard) -> (movement_phase_loop_bot(NewBoard,Size,PColor))
        ;
        append([], Board, NewBoard),
        movement_phase_loop_bot(NewBoard,Size,PColor)
        ))
    ).


get_random_color(Color) :-
        random(0,2, C),
        (C == 0 -> Color = w; Color = b).
        
get_random_play(Board,Size,X1,Y1) :-
        random(0,Size, X1),
        nth0(X1, Board, Row),
        length(Row, RowSize),
        random(0,RowSize, Y1).

get_random_piece(Board,Size,X1,Y1) :-
        get_random_play(Board,Size,X1,Y1),
        check_piece_ownership([X1,Y1]).

win_conditioning_check(Board) :-
        check_win(Board, w),
        check_win(Board, b).
game_over(Board, Winner) :-
        (check_win(Board, 1),
         Winner = white)
        ;
        (check_win(Board, 2),
         Winner = black).

check_win(Board, Colour) :-
        check_each_row(Board, Colour);
        check_diagonals(Board, Colour).
      